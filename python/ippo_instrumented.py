import numpy as np
import torch
from gymnasium import spaces
from torch.nn.utils import clip_grad_norm_

from agilerl.algorithms.ippo import IPPO
from agilerl.algorithms.core import OptimizerWrapper
from agilerl.modules import EvolvableModule
from agilerl.networks.actors import StochasticActor
from agilerl.networks.value_networks import ValueNetwork
from agilerl.typing import ExperiencesType, StandardTensorDict
from agilerl.utils.algo_utils import (
    concatenate_experiences_into_batches,
    get_experiences_samples,
    vectorize_experiences_by_agent,
)
from agilerl.utils.algo_utils import (
    preprocess_observation as preprocess_observation_fn,
)

from training_metrics import (
    compute_clip_fraction,
    compute_relative_entropy,
    compute_residual_variance,
    tensor_histogram,
    tensor_stats,
)


class InstrumentedIPPO(IPPO):
    """IPPO subclass that returns rich training metrics from learn()."""

    def learn(self, experiences: ExperiencesType) -> StandardTensorDict:
        states, actions, log_probs, rewards, dones, values, next_states, next_dones = (
            map(self.assemble_shared_inputs, experiences)
        )

        loss_dict = {}
        self.last_metrics: dict[str, dict] = {}
        for agent_id, state in states.items():
            actor = self.actors[agent_id]
            critic = self.critics[agent_id]
            actor_optimizer = self.actor_optimizers[agent_id]
            critic_optimizer = self.critic_optimizers[agent_id]
            obs_space = self.observation_space[agent_id]
            action_space = self.action_space[agent_id]

            loss, metrics = self._learn_individual_instrumented(
                experiences=(
                    state,
                    actions[agent_id],
                    log_probs[agent_id],
                    rewards[agent_id],
                    dones[agent_id],
                    values[agent_id],
                    next_states[agent_id],
                    next_dones[agent_id],
                ),
                actor=actor,
                critic=critic,
                actor_optimizer=actor_optimizer,
                critic_optimizer=critic_optimizer,
                obs_space=obs_space,
                action_space=action_space,
            )
            loss_dict[f"{agent_id}"] = loss
            self.last_metrics[agent_id] = metrics

        return loss_dict

    def _learn_individual_instrumented(
        self,
        experiences: ExperiencesType,
        actor: EvolvableModule | StochasticActor,
        critic: EvolvableModule | ValueNetwork,
        actor_optimizer: OptimizerWrapper,
        critic_optimizer: OptimizerWrapper,
        obs_space: spaces,
        action_space: spaces,
    ) -> tuple[float, dict]:
        """PPO learn step with full metric instrumentation.

        Returns (loss, metrics_dict) instead of just loss.
        """
        states, actions, log_probs, rewards, dones, values, next_state, next_done = (
            experiences
        )

        log_probs, rewards, dones, values = map(
            vectorize_experiences_by_agent,
            (log_probs, rewards, dones, values),
        )
        log_probs = log_probs.squeeze()
        rewards = rewards.squeeze()
        dones = dones.squeeze()
        values = values.squeeze()
        next_state = vectorize_experiences_by_agent(next_state, dim=0)
        next_done = vectorize_experiences_by_agent(next_done, dim=0)

        with torch.no_grad():
            num_steps = rewards.size(0)
            rewards = rewards.reshape(num_steps, -1)
            dones = dones.reshape(num_steps, -1)
            values = values.reshape(num_steps, -1)
            next_done = next_done.reshape(1, -1)

            next_state = preprocess_observation_fn(
                obs_space,
                next_state,
                self.device,
                self.normalize_images,
            )
            next_value = critic(next_state).reshape(1, -1).cpu()
            advantages = torch.zeros_like(rewards).float()
            last_gae_lambda = 0
            for t in reversed(range(num_steps)):
                if t == num_steps - 1:
                    next_non_terminal = 1.0 - next_done
                    nextvalue = next_value.squeeze()
                else:
                    next_non_terminal = 1.0 - dones[t + 1]
                    nextvalue = values[t + 1]

                delta = (
                    rewards[t] + self.gamma * nextvalue * next_non_terminal - values[t]
                )
                advantages[t] = last_gae_lambda = (
                    delta
                    + self.gamma * self.gae_lambda * next_non_terminal * last_gae_lambda
                )

            advantages = advantages.reshape((-1,))
            values = values.reshape((-1,))
            returns = advantages + values

        # Collect pre-update metrics on advantages and returns
        reward_stats = tensor_stats(rewards)
        advantage_stats = tensor_stats(advantages)
        value_target_stats = tensor_stats(returns)

        states = concatenate_experiences_into_batches(states, obs_space)
        actions = concatenate_experiences_into_batches(
            actions,
            action_space,
            actions=True,
        )
        log_probs = log_probs.reshape((-1,))
        experiences = (states, actions, log_probs, advantages, returns, values)

        # Move experiences to algo device
        experiences = self.to_device(*experiences)

        # Pre-update residual variance (before any gradient steps)
        with torch.no_grad():
            pre_states = preprocess_observation_fn(
                obs_space, experiences[0], self.device, self.normalize_images
            )
            pre_update_values = critic(pre_states).squeeze(-1)
            pre_update_resvar = compute_residual_variance(pre_update_values, experiences[4])
            hist_value_targets = tensor_histogram(experiences[4])
            hist_rewards = tensor_histogram(rewards)
            hist_values = tensor_histogram(pre_update_values)

        num_samples = experiences[4].size(0)
        batch_idxs = np.arange(num_samples)
        mean_loss = 0
        approx_kl = torch.tensor(float("inf"))

        # Accumulators for per-minibatch metrics
        all_kl = []
        all_clip_frac = []
        all_entropy = []
        all_pg_loss = []
        all_v_loss = []
        all_actor_grad_norm = []
        all_critic_grad_norm = []
        num_updates = 0

        for _ in range(self.update_epochs):
            np.random.shuffle(batch_idxs)
            for start in range(0, num_samples, self.batch_size):
                minibatch_idxs = batch_idxs[start : start + self.batch_size]
                (
                    batch_states,
                    batch_actions,
                    batch_log_probs,
                    batch_advantages,
                    batch_returns,
                    batch_values,
                ) = get_experiences_samples(minibatch_idxs, *experiences)

                batch_actions = batch_actions.squeeze()
                batch_returns = batch_returns.squeeze()
                batch_log_probs = batch_log_probs.squeeze()
                batch_advantages = batch_advantages.squeeze()
                batch_values = batch_values.squeeze()

                if len(minibatch_idxs) > 1:
                    batch_states = preprocess_observation_fn(
                        obs_space,
                        batch_states,
                        self.device,
                        self.normalize_images,
                    )
                    _, _, entropy = actor(batch_states)
                    value = critic(batch_states).squeeze(-1)

                    log_prob = actor.action_log_prob(batch_actions)

                    logratio = log_prob - batch_log_probs
                    ratio = logratio.exp()

                    with torch.no_grad():
                        approx_kl = ((ratio - 1) - logratio).mean()
                        clip_frac = compute_clip_fraction(ratio, self.clip_coef)

                    minibatch_advs = batch_advantages
                    minibatch_advs = (minibatch_advs - minibatch_advs.mean()) / (
                        minibatch_advs.std() + 1e-8
                    )

                    # Policy loss
                    pg_loss1 = -minibatch_advs * ratio
                    pg_loss2 = -minibatch_advs * torch.clamp(
                        ratio,
                        1 - self.clip_coef,
                        1 + self.clip_coef,
                    )
                    pg_loss = torch.max(pg_loss1, pg_loss2).mean()

                    # Value loss
                    value = value.view(-1)
                    v_loss_unclipped = (value - batch_returns) ** 2
                    v_clipped = batch_values + torch.clamp(
                        value - batch_values,
                        -self.clip_coef,
                        self.clip_coef,
                    )

                    v_loss_clipped = (v_clipped - batch_returns) ** 2
                    v_loss_max = torch.max(v_loss_unclipped, v_loss_clipped)
                    v_loss = 0.5 * v_loss_max.mean()

                    entropy_loss = entropy.mean()

                    actor_loss = pg_loss - self.ent_coef * entropy_loss
                    critic_loss = v_loss * self.vf_coef

                    # Actor backward + grad norm
                    actor_optimizer.zero_grad()
                    if self.accelerator is not None:
                        self.accelerator.backward(actor_loss)
                    else:
                        actor_loss.backward()

                    actor_grad_norm = clip_grad_norm_(
                        actor.parameters(), self.max_grad_norm
                    )
                    actor_optimizer.step()

                    # Critic backward + grad norm
                    critic_optimizer.zero_grad()
                    if self.accelerator is not None:
                        self.accelerator.backward(critic_loss)
                    else:
                        critic_loss.backward()
                    critic_grad_norm = clip_grad_norm_(
                        critic.parameters(), self.max_grad_norm
                    )
                    critic_optimizer.step()

                    mean_loss += actor_loss.item() + critic_loss.item()

                    # Collect metrics
                    all_kl.append(approx_kl.item())
                    all_clip_frac.append(clip_frac)
                    all_entropy.append(entropy_loss.item())
                    all_pg_loss.append(pg_loss.item())
                    all_v_loss.append(v_loss.item())
                    all_actor_grad_norm.append(actor_grad_norm.item())
                    all_critic_grad_norm.append(critic_grad_norm.item())
                    num_updates += 1

            if self.target_kl is not None and approx_kl > self.target_kl:
                break

        mean_loss /= num_samples * self.update_epochs

        # Determine action space size for relative entropy
        if isinstance(action_space, spaces.Discrete):
            action_space_size = action_space.n
        elif isinstance(action_space, spaces.MultiDiscrete):
            action_space_size = int(np.prod(action_space.nvec))
        else:
            action_space_size = 0

        # Build metrics dict
        metrics = {
            "rewards": reward_stats,
            "advantages": advantage_stats,
            "value_targets": value_target_stats,
        }

        if num_updates > 0:
            metrics.update({
                "approx_kl": float(np.mean(all_kl)),
                "clip_fraction": float(np.mean(all_clip_frac)),
                "entropy": float(np.mean(all_entropy)),
                "relative_entropy": (
                    compute_relative_entropy(
                        torch.tensor(all_entropy), action_space_size
                    )
                    if action_space_size > 0
                    else None
                ),
                "policy_loss": float(np.mean(all_pg_loss)),
                "value_loss": float(np.mean(all_v_loss)),
                "residual_variance": pre_update_resvar,
                "hist_value_targets": hist_value_targets,
                "hist_rewards": hist_rewards,
                "hist_values": hist_values,
                "actor_grad_norm": float(np.mean(all_actor_grad_norm)),
                "critic_grad_norm": float(np.mean(all_critic_grad_norm)),
                "num_updates": num_updates,
            })

        return mean_loss, metrics
