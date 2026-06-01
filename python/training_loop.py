"""Forked from agilerl.training.train_multi_agent_on_policy with:
- Rich training metrics (from InstrumentedIPPO)
- JSON lines logging to metrics.jsonl
- Fixed progress bar step counting
- Episode length tracking
"""

import json
import time
import warnings
from copy import deepcopy
from datetime import datetime
from pathlib import Path
from typing import TYPE_CHECKING, Any

import numpy as np
import wandb
from accelerate import Accelerator
from gymnasium import spaces
from pettingzoo import ParallelEnv

from agilerl.algorithms import IPPO
from agilerl.hpo.mutation import Mutations
from agilerl.hpo.tournament import TournamentSelection
from agilerl.networks import StochasticActor
from agilerl.utils.algo_utils import obs_channels_to_first
from agilerl.utils.utils import (
    default_progress_bar,
    init_wandb,
    save_population_checkpoint,
    tournament_selection_and_mutation,
)
from agilerl.vector.pz_async_vec_env import AsyncPettingZooVecEnv

if TYPE_CHECKING:
    from agilerl.typing import SingleAgentModule

InitDictType = dict[str, Any] | None
MultiAgentOnPolicyAlgorithms = IPPO
PopulationType = list[MultiAgentOnPolicyAlgorithms]


def train_multi_agent_on_policy(
    env: ParallelEnv | AsyncPettingZooVecEnv,
    env_name: str,
    algo: str,
    pop: PopulationType,
    sum_scores: bool = True,
    INIT_HP: InitDictType = None,
    MUT_P: InitDictType = None,
    swap_channels: bool = False,
    max_steps: int = 50000,
    evo_steps: int = 25,
    eval_steps: int | None = None,
    eval_loop: int = 1,
    target: float | None = None,
    tournament: TournamentSelection | None = None,
    mutation: Mutations | None = None,
    checkpoint: int | None = None,
    checkpoint_path: str | None = None,
    overwrite_checkpoints: bool = False,
    save_elite: bool = False,
    elite_path: str | None = None,
    wb: bool = False,
    verbose: bool = True,
    accelerator: Accelerator | None = None,
    wandb_api_key: str | None = None,
    log_dir: str | None = None,
) -> tuple[PopulationType, list[list[float]]]:
    """Multi-agent on-policy training with rich metric logging.

    Same interface as agilerl's version, plus:
    - log_dir: directory for metrics.jsonl (defaults to checkpoint_path or 'runs/')
    """
    assert isinstance(algo, str)
    assert isinstance(max_steps, int)
    assert isinstance(evo_steps, int)
    if target is not None:
        assert isinstance(target, (float, int))
    if checkpoint is not None:
        assert isinstance(checkpoint, int)
    assert isinstance(wb, bool)
    assert isinstance(verbose, bool)
    if save_elite is False and elite_path is not None:
        warnings.warn(
            "'save_elite' set to False but 'elite_path' has been defined, elite will not"
            " be saved unless 'save_elite' is set to True.",
            stacklevel=2,
        )
    if checkpoint is None and checkpoint_path is not None:
        warnings.warn(
            "'checkpoint' set to None but 'checkpoint_path' has been defined, checkpoint will not"
            " be saved unless 'checkpoint' is defined.",
            stacklevel=2,
        )

    start_time = time.time()

    if wb:
        init_wandb(
            algo=algo,
            env_name=env_name,
            init_hyperparams=INIT_HP,
            mutation_hyperparams=MUT_P,
            wandb_api_key=wandb_api_key,
            project="AgileRLMultiAgent",
            accelerator=accelerator,
        )

    if hasattr(env, "num_envs"):
        is_vectorised = True
        num_envs = env.num_envs
    else:
        is_vectorised = False
        num_envs = 1

    save_path = (
        checkpoint_path.split(".pt")[0]
        if checkpoint_path is not None
        else "{}-EvoHPO-{}-{}".format(
            env_name,
            algo,
            datetime.now().strftime("%m%d%Y%H%M%S"),
        )
    )

    # Set up JSON metrics log
    metrics_dir = Path(log_dir or checkpoint_path or "runs")
    metrics_dir.mkdir(parents=True, exist_ok=True)
    metrics_file = open(metrics_dir / "metrics.jsonl", "a")

    if accelerator is not None:
        print(f"\nDistributed training on {accelerator.device}...")
    else:
        print("\nTraining...")

    # Format progress bar
    pbar = default_progress_bar(max_steps, accelerator)

    sample_ind = pop[0]
    agent_ids = deepcopy(list(sample_ind.observation_space.keys()))
    pop_loss = [{agent_id: [] for agent_id in agent_ids} for _ in pop]
    pop_fitnesses = [{agent_id: [] for agent_id in agent_ids} for _ in pop]
    entropy_hist = [{agent_id: [] for agent_id in agent_ids} for _ in pop]
    total_steps = 0
    loss = None
    checkpoint_count = 0

    # Pre-training mutation
    if accelerator is None and mutation is not None:
        pop = mutation.mutation(pop, pre_training_mut=True)

    # RL training loop
    while np.sum([agent.steps[-1] for agent in pop]) < max_steps:
        if accelerator is not None:
            accelerator.wait_for_everyone()

        pop_episode_scores = []
        pop_fps = []
        for agent_idx, agent in enumerate(pop):  # Loop through population
            compiled_agent = agent.torch_compiler is not None
            agent.set_training_mode(True)

            obs, info = env.reset()
            scores = (
                np.zeros((num_envs, 1))
                if sum_scores
                else np.zeros((num_envs, len(agent_ids)))
            )
            losses = {agent_id: [] for agent_id in agent_ids}
            completed_episode_scores = []
            episode_lengths = []
            current_episode_length = 0
            steps = 0
            if swap_channels:
                expand_dims = not is_vectorised
                obs = {
                    agent_id: obs_channels_to_first(s, expand_dims)
                    for agent_id, s in obs.items()
                }

            agent_start_time = time.time()
            for _ in range(-(evo_steps // -agent.learn_step)):
                states = {agent_id: [] for agent_id in agent.agent_ids}
                actions = {agent_id: [] for agent_id in agent.agent_ids}
                log_probs = {agent_id: [] for agent_id in agent.agent_ids}
                entropies = {agent_id: [] for agent_id in agent.agent_ids}
                rewards = {agent_id: [] for agent_id in agent.agent_ids}
                dones = {agent_id: [] for agent_id in agent.agent_ids}
                values = {agent_id: [] for agent_id in agent.agent_ids}

                done = {agent_id: np.zeros(num_envs) for agent_id in agent.agent_ids}

                for _ in range(-(agent.learn_step // -num_envs)):
                    action, log_prob, entropy, value = agent.get_action(
                        obs=obs,
                        infos=info,
                    )

                    if not is_vectorised:
                        action = {agent: act[0] for agent, act in action.items()}
                        log_prob = {agent: lp[0] for agent, lp in log_prob.items()}
                        entropy = {agent: ent[0] for agent, ent in entropy.items()}
                        value = {agent: val[0] for agent, val in value.items()}

                    # Clip to action space
                    clipped_action = {}
                    for agent_id, agent_action in action.items():
                        network_id = (
                            agent_id
                            if agent_id in agent.actors
                            else agent.get_group_id(agent_id)
                        )
                        agent_space = agent.possible_action_spaces[agent_id]
                        policy = getattr(agent, agent.registry.policy())
                        agent_policy: SingleAgentModule = policy[network_id]

                        if compiled_agent:
                            agent_policy = agent_policy._orig_mod

                        if isinstance(agent_policy, StochasticActor) and isinstance(
                            agent_space,
                            spaces.Box,
                        ):
                            if agent_policy.squash_output:
                                clipped_agent_action = agent_policy.scale_action(
                                    agent_action,
                                )
                            else:
                                clipped_agent_action = np.clip(
                                    agent_action,
                                    agent_space.low,
                                    agent_space.high,
                                )
                        else:
                            clipped_agent_action = agent_action

                        clipped_action[agent_id] = clipped_agent_action

                    next_obs, reward, termination, truncation, info = env.step(
                        clipped_action,
                    )

                    agent_rewards = np.array(list(reward.values())).transpose()
                    agent_rewards = np.where(np.isnan(agent_rewards), 0, agent_rewards)
                    score_increment = (
                        (
                            np.sum(agent_rewards, axis=-1)[:, np.newaxis]
                            if is_vectorised
                            else np.sum(agent_rewards, axis=-1)
                        )
                        if sum_scores
                        else agent_rewards
                    )
                    scores += score_increment
                    total_steps += num_envs
                    steps += num_envs
                    current_episode_length += 1

                    for agent_id in obs:
                        # Only record experiences for agents that were active
                        # (AsyncAgentsWrapper removes inactive agents from action dicts)
                        if agent_id not in action:
                            continue
                        states[agent_id].append(obs[agent_id])
                        rewards[agent_id].append(reward[agent_id])
                        actions[agent_id].append(action[agent_id])
                        log_probs[agent_id].append(log_prob[agent_id])
                        entropies[agent_id].append(entropy[agent_id])
                        values[agent_id].append(value[agent_id])
                        dones[agent_id].append(done[agent_id])

                    next_done = {}
                    for agent_id in termination:
                        terminated = termination[agent_id]
                        truncated = truncation[agent_id]

                        if is_vectorised:
                            mask = ~(np.isnan(terminated) | np.isnan(truncated))
                            result = np.full_like(mask, np.nan, dtype=float)
                            result[mask] = np.logical_or(
                                terminated[mask],
                                truncated[mask],
                            )
                            next_done[agent_id] = result
                        else:
                            next_done[agent_id] = np.array(
                                [np.logical_or(terminated, truncated)],
                            ).astype(np.int8)

                    if swap_channels:
                        expand_dims = not is_vectorised
                        next_obs = {
                            agent_id: obs_channels_to_first(s, expand_dims)
                            for agent_id, s in next_obs.items()
                        }

                    obs = next_obs
                    done = next_done
                    for idx, agent_dones in enumerate(
                        zip(*next_done.values(), strict=False)
                    ):
                        if all(agent_dones):
                            completed_score = (
                                float(scores[idx].item())
                                if sum_scores
                                else list(scores[idx])
                            )
                            completed_episode_scores.append(completed_score)
                            agent.scores.append(completed_score)
                            scores[idx].fill(0)
                            episode_lengths.append(current_episode_length)
                            current_episode_length = 0
                            if not is_vectorised:
                                obs, info = env.reset()

                            done = {
                                agent_id: np.zeros(num_envs)
                                for agent_id in agent.agent_ids
                            }

                experiences = (
                    states,
                    actions,
                    log_probs,
                    rewards,
                    dones,
                    values,
                    next_obs,
                    next_done,
                )

                loss = agent.learn(experiences)

                if agent.has_grouped_agents():
                    entropies = agent.assemble_grouped_outputs(entropies, num_envs)

                for agent_id in agent_ids:
                    losses[agent_id].append(loss[agent_id])
                    entropy_hist[agent_idx][agent_id].append(
                        np.mean(entropies[agent_id]),
                    )

            agent.steps[-1] += steps
            # Fix: update pbar by actual steps collected, not evo_steps estimate
            pbar.update(steps)
            elapsed = max(time.time() - agent_start_time, 1e-12)
            fps = steps / elapsed

            pop_fps.append(fps)
            pop_episode_scores.append(completed_episode_scores)
            if len(losses[agent_ids[0]]) > 0:
                if all(losses[a_id] for a_id in agent_ids):
                    for agent_id in agent_ids:
                        unique_loss = [
                            loss for loss in losses[agent_id] if loss is not None
                        ]
                        pop_loss[agent_idx][agent_id].append(np.mean(unique_loss))

            # Log metrics from instrumented agent
            if hasattr(agent, "last_metrics") and agent.last_metrics:
                for agent_id, metrics in agent.last_metrics.items():
                    log_entry = {
                        "global_step": total_steps,
                        "wall_time": time.time() - start_time,
                        "agent_idx": agent_idx,
                        "agent_id": agent_id,
                        "fps": fps,
                        **metrics,
                    }
                    if episode_lengths:
                        log_entry["episode_length"] = {
                            "mean": float(np.mean(episode_lengths)),
                            "std": float(np.std(episode_lengths)) if len(episode_lengths) > 1 else 0.0,
                            "min": int(np.min(episode_lengths)),
                            "max": int(np.max(episode_lengths)),
                            "count": len(episode_lengths),
                        }
                    if completed_episode_scores:
                        scores_arr = np.array(completed_episode_scores)
                        log_entry["episode_score"] = {
                            "mean": float(scores_arr.mean()),
                            "std": float(scores_arr.std()) if len(scores_arr) > 1 else 0.0,
                            "min": float(scores_arr.min()),
                            "max": float(scores_arr.max()),
                        }
                    metrics_file.write(json.dumps(log_entry) + "\n")
                    metrics_file.flush()

                # Console summary (compact)
                first_agent = next(iter(agent.last_metrics))
                m = agent.last_metrics[first_agent]
                ent_str = f"entropy={m.get('relative_entropy', m.get('entropy', '?')):.3f}" if 'entropy' in m or 'relative_entropy' in m else ""
                kl_str = f"kl={m.get('approx_kl', '?'):.4f}" if 'approx_kl' in m else ""
                rv_str = f"resvar={m.get('residual_variance', '?'):.3f}" if 'residual_variance' in m else ""
                vl_str = f"vloss={m.get('value_loss', '?'):.4f}" if 'value_loss' in m else ""
                cf_str = f"clip={m.get('clip_fraction', '?'):.3f}" if 'clip_fraction' in m else ""
                score_str = ""
                if completed_episode_scores:
                    score_str = f"score={np.mean(completed_episode_scores):.1f}"

                parts = [p for p in [ent_str, kl_str, rv_str, vl_str, cf_str, score_str] if p]
                if parts:
                    pbar.write(f"  [step {total_steps}] agent {agent_idx}: {' | '.join(parts)}")

        # Evaluate population
        fitnesses = [
            agent.test(
                env,
                swap_channels=swap_channels,
                max_steps=eval_steps,
                loop=eval_loop,
                sum_scores=sum_scores,
            )
            for agent in pop
        ]
        pop_fitnesses.append(fitnesses)
        if sum_scores:
            mean_scores = [
                (
                    np.mean(episode_scores)
                    if len(episode_scores) > 0
                    else "0 completed episodes"
                )
                for episode_scores in pop_episode_scores
            ]
            mean_score_dict = {
                "train/mean_score": np.mean(
                    [
                        mean_score
                        for mean_score in mean_scores
                        if not isinstance(mean_score, str)
                    ],
                ),
            }
            fitness_dict = {
                "eval/mean_fitness": np.mean(fitnesses),
                "eval/best_fitness": np.max(fitnesses),
            }
        else:
            pop_mean_scores = [
                np.mean(np.array(score), axis=0)
                for score in pop_episode_scores
                if score
            ]
            if pop_episode_scores:
                mean_scores = np.stack(pop_mean_scores, axis=0)
                mean_score_dict = {
                    "train/mean_score/" + agent: np.mean(mean_scores[:, idx], axis=-1)
                    for idx, agent in enumerate(agent_ids)
                }
            else:
                mean_score_dict = {
                    "train/mean_score/" + agent: np.nan
                    for idx, agent in enumerate(agent_ids)
                }
            mean_fitnesses = np.mean(fitnesses, axis=0)
            max_fitnesses = np.max(fitnesses, axis=0)
            fitness_dict = {
                "eval/mean_fitness/" + agent: mean_fitnesses[idx]
                for idx, agent in enumerate(agent_ids)
            }
            best_fitness_dict = {
                "eval/best_fitness/" + agent: max_fitnesses[idx]
                for idx, agent in enumerate(agent_ids)
            }
            fitness_dict.update(best_fitness_dict)

        if wb:
            wandb_dict = {
                "global_step": (
                    total_steps * accelerator.state.num_processes
                    if accelerator is not None and accelerator.is_main_process
                    else total_steps
                ),
                "fps": np.mean(pop_fps),
            }
            wandb_dict.update(fitness_dict)
            wandb_dict.update(mean_score_dict)

            loss_dict = {}
            entropy_dict = {}

            for agent_idx, _ in enumerate(pop):
                for agent_id, loss in zip(
                    pop_loss[agent_idx].keys(),
                    pop_loss[agent_idx].values(),
                    strict=False,
                ):
                    loss_dict[f"train/agent_{agent_idx}_{agent_id}_loss"] = np.mean(
                        loss[-10:],
                    )
                    wandb_dict.update(loss_dict)

                for agent_id, entropy_values in zip(
                    entropy_hist[agent_idx].keys(),
                    entropy_hist[agent_idx].values(),
                    strict=False,
                ):
                    if entropy_values:
                        entropy_dict[f"train/agent_{agent_idx}_{agent_id}_entropy"] = (
                            np.mean(entropy_values[-10:])
                        )
                    wandb_dict.update(entropy_dict)

            if accelerator is not None:
                accelerator.wait_for_everyone()
                if accelerator.is_main_process:
                    wandb.log(wandb_dict)
                accelerator.wait_for_everyone()
            else:
                wandb.log(wandb_dict)

            for idx, agent in enumerate(pop):
                wandb.log(
                    {
                        f"learn_step_agent_{idx}": agent.learn_step,
                        f"learning_rate_agent_{idx}": agent.lr,
                        f"batch_size_agent_{idx}": agent.batch_size,
                        f"indi_fitness_agent_{idx}": agent.fitness[-1],
                    },
                )

        # Update step counter
        for agent in pop:
            agent.steps.append(agent.steps[-1])

        # Early stop if consistently reaches target
        if target is not None and (
            np.all(
                np.greater([np.mean(agent.fitness[-10:]) for agent in pop], target),
            )
            and len(pop[0].steps) >= 100
        ):
            if wb:
                wandb.finish()
            metrics_file.close()
            return pop, pop_fitnesses

        # Tournament selection and population mutation
        if tournament and mutation is not None:
            pop = tournament_selection_and_mutation(
                population=pop,
                tournament=tournament,
                mutation=mutation,
                env_name=env_name,
                algo=algo,
                elite_path=elite_path,
                save_elite=save_elite,
                accelerator=accelerator,
            )

        if verbose:
            if sum_scores:
                fitness = [f"{fitness:.2f}" for fitness in fitnesses]
                avg_fitness = [f"{np.mean(agent.fitness[-5:]):.2f}" for agent in pop]
                avg_score = [f"{np.mean(agent.scores[-10:]):.2f}" for agent in pop]
                mean_scores = [
                    (
                        f"{mean_score:.2f}"
                        if not isinstance(mean_score, str)
                        else mean_score
                    )
                    for mean_score in mean_scores
                ]
            else:
                fitness_arr = np.array(list(fitnesses))
                avg_fitness_arr = np.array(
                    [np.mean(agent.fitness[-5:], axis=0) for agent in pop],
                )
                avg_score_arr = np.array(
                    [np.mean(agent.scores[-10:], axis=0) for agent in pop],
                )
                fitness = {
                    agent: fitness_arr[:, idx] for idx, agent in enumerate(agent_ids)
                }
                avg_fitness = {
                    agent: avg_fitness_arr[:, idx] for idx, agent in enumerate(agent_ids)
                }
                avg_score = {
                    agent: avg_score_arr[:, idx] for idx, agent in enumerate(agent_ids)
                }
                mean_scores = {
                    agent: mean_scores[:, idx] for idx, agent in enumerate(agent_ids)
                }

            agents = [agent.index for agent in pop]
            num_steps = [agent.steps[-1] for agent in pop]
            muts = [agent.mut for agent in pop]

            banner_text = f"Global Steps {total_steps}"
            banner_width = max(len(banner_text) + 8, 35)
            border = "=" * banner_width
            centered_text = f"{banner_text}".center(banner_width)
            pbar.write(
                f"{border}\n"
                f"{centered_text}\n"
                f"{border}\n"
                f"Fitness:\t{fitness}\n"
                f"Score:\t\t{mean_scores}\n"
                f"5 fitness avgs:\t{avg_fitness}\n"
                f"10 score avgs:\t{avg_score}\n"
                f"Agents:\t\t{agents}\n"
                f"Steps:\t\t{num_steps}\n"
                f"Mutations:\t{muts}",
            )

        # Save model checkpoint
        if checkpoint is not None:
            if pop[0].steps[-1] // checkpoint > checkpoint_count:
                save_population_checkpoint(
                    population=pop,
                    save_path=save_path,
                    overwrite_checkpoints=overwrite_checkpoints,
                    accelerator=accelerator,
                )
                checkpoint_count += 1

    if wb:
        if accelerator is not None:
            accelerator.wait_for_everyone()
            if accelerator.is_main_process:
                wandb.finish()
            accelerator.wait_for_everyone()
        else:
            wandb.finish()

    metrics_file.close()
    pbar.close()
    return pop, pop_fitnesses
