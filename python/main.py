import sys
import yaml
import numpy as np
import torch

from training_loop import train_multi_agent_on_policy
from ippo_instrumented import InstrumentedIPPO
from agilerl.algorithms.core.registry import HyperparameterConfig, RLParameter
from agilerl.hpo.mutation import Mutations
from agilerl.hpo.tournament import TournamentSelection
from agilerl.vector.pz_async_vec_env import AsyncPettingZooVecEnv
from agilerl.wrappers.agent import AsyncAgentsWrapper  # base class for DictSafeAsyncAgentsWrapper

from haskboard_env import make


def _is_all_nan(obs) -> bool:
    """Check if an observation (array or dict of arrays) is entirely NaN."""
    if obs is None:
        return True
    if isinstance(obs, dict):
        return all(_is_all_nan(v) for v in obs.values())
    return np.isnan(obs).all()


def _index_obs(obs, idx):
    """Index into a stacked observation (array or dict of arrays)."""
    if isinstance(obs, dict):
        return {k: v[idx] for k, v in obs.items()}
    return obs[idx]


def _slice_obs(obs, s):
    """Slice a stacked observation (array or dict of arrays)."""
    if isinstance(obs, dict):
        return {k: v[s] for k, v in obs.items()}
    return obs[s]


class DictSafeAsyncAgentsWrapper(AsyncAgentsWrapper):
    """Fix AsyncAgentsWrapper for Dict obs spaces and vectorized turn-based games.

    Fixes:
    1. learn(): np.isnan(next_state) and states[-1] indexing fail on dict obs.
    2. get_action(): with num_envs>1, turn-based games create ragged batch sizes
       per agent (different agents active in different env subsets), crashing
       IPPO's disassemble_grouped_outputs. We sanitize NaN→0 so all agents
       process all num_envs uniformly.
    """

    def get_action(self, obs, *args, **kwargs):
        for agent_id in obs:
            agent_obs = obs[agent_id]
            if isinstance(agent_obs, dict):
                for k in agent_obs:
                    agent_obs[k] = np.nan_to_num(agent_obs[k], nan=0.0)
            elif isinstance(agent_obs, np.ndarray):
                obs[agent_id] = np.nan_to_num(agent_obs, nan=0.0)
        return super().get_action(obs, *args, **kwargs)

    def learn(self, experiences, *args, **kwargs):
        if self.agent.algo in {"MADDPG", "MATD3"}:
            experiences = self.stack_experiences(experiences)
            experiences = self._align_async_off_policy_experiences(experiences)
            return self.wrapped_learn(experiences, *args, **kwargs)

        states, actions, log_probs, rewards, dones, values, next_state, next_done = map(
            self.stack_experiences,
            experiences,
        )

        for agent_id in self.agent.agent_ids:
            agent_next_state = next_state.get(agent_id, None)

            if _is_all_nan(agent_next_state):
                agent_states = states[agent_id]
                agent_dones = dones[agent_id]
                agent_rewards = rewards[agent_id]

                next_state[agent_id] = _index_obs(agent_states, -1)
                next_done[agent_id] = agent_dones[-1]
                states[agent_id] = _slice_obs(agent_states, slice(None, -1))
                dones[agent_id] = agent_dones[:-1]
                rewards[agent_id] = agent_rewards[:-1]
                actions[agent_id] = actions[agent_id][:-1]
                log_probs[agent_id] = log_probs[agent_id][:-1]
                values[agent_id] = values[agent_id][:-1]

        experiences = (
            states, actions, log_probs, rewards, dones, values, next_state, next_done,
        )
        return self.wrapped_learn(experiences, *args, **kwargs)

sys.stdout.reconfigure(line_buffering=True)  # type: ignore[union-attr]


def main(INIT_HP, MUTATION_PARAMS, NET_CONFIG):
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

    BINARY = "/home/adam/haskell/haskboard/dist-newstyle/build/x86_64-linux/ghc-9.10.1/NoMerci-0.1.0.0/x/NoMerci/build/NoMerci/NoMerci"
    num_envs = INIT_HP.get("NUM_ENVS", 1)
    env = AsyncPettingZooVecEnv(
        [lambda: make(BINARY, shared=False) for _ in range(num_envs)]
    )

    # Extract single (unbatched) spaces for agent construction
    obs_spaces = {a: env.single_observation_space(a) for a in env.possible_agents}
    act_spaces = {a: env.single_action_space(a) for a in env.possible_agents}

    INIT_HP["AGENT_IDS"] = env.possible_agents

    tournament = TournamentSelection(
        INIT_HP["TOURN_SIZE"],
        INIT_HP["ELITISM"],
        INIT_HP["POP_SIZE"],
        INIT_HP["EVAL_LOOP"],
    )

    mutations = Mutations(
        no_mutation=MUTATION_PARAMS["NO_MUT"],
        architecture=MUTATION_PARAMS["ARCH_MUT"],
        new_layer_prob=MUTATION_PARAMS["NEW_LAYER"],
        parameters=MUTATION_PARAMS["PARAMS_MUT"],
        activation=MUTATION_PARAMS["ACT_MUT"],
        rl_hp=MUTATION_PARAMS["RL_HP_MUT"],
        mutation_sd=MUTATION_PARAMS["MUT_SD"],
        rand_seed=MUTATION_PARAMS["RAND_SEED"],
        device=device,
    )

    hp_config = HyperparameterConfig(
        lr=RLParameter(min=MUTATION_PARAMS["MIN_LR"], max=MUTATION_PARAMS["MAX_LR"]),
        batch_size=RLParameter(
            min=MUTATION_PARAMS["MIN_BATCH_SIZE"],
            max=MUTATION_PARAMS["MAX_BATCH_SIZE"],
            dtype=int,
        ),
        learn_step=RLParameter(
            min=MUTATION_PARAMS["MIN_LEARN_STEP"],
            max=MUTATION_PARAMS["MAX_LEARN_STEP"],
            dtype=int,
            grow_factor=1.5,
            shrink_factor=0.75,
        ),
        ent_coef=RLParameter(
            min=MUTATION_PARAMS["MIN_ENT_COEF"],
            max=MUTATION_PARAMS["MAX_ENT_COEF"],
        ),
    )

    pop = [
        DictSafeAsyncAgentsWrapper(InstrumentedIPPO(
            observation_spaces=obs_spaces,
            action_spaces=act_spaces,
            agent_ids=INIT_HP["AGENT_IDS"],
            index=idx,
            hp_config=hp_config,
            net_config=NET_CONFIG,
            batch_size=INIT_HP.get("BATCH_SIZE", 64),
            lr=INIT_HP.get("LR", 0.0001),
            learn_step=INIT_HP.get("LEARN_STEP", 2048),
            gamma=INIT_HP.get("GAMMA", 0.99),
            gae_lambda=INIT_HP.get("GAE_LAMBDA", 0.95),
            action_std_init=INIT_HP.get("ACTION_STD_INIT", 0.0),
            clip_coef=INIT_HP.get("CLIP_COEF", 0.2),
            ent_coef=INIT_HP.get("ENT_COEF", 0.01),
            vf_coef=INIT_HP.get("VF_COEF", 0.5),
            max_grad_norm=INIT_HP.get("MAX_GRAD_NORM", 0.5),
            target_kl=INIT_HP.get("TARGET_KL"),
            update_epochs=INIT_HP.get("UPDATE_EPOCHS", 4),
            device=device,
            torch_compiler=INIT_HP["TORCH_COMPILE"],
        ))
        for idx in range(INIT_HP["POP_SIZE"])
    ]

    train_multi_agent_on_policy(
        env,
        INIT_HP["ENV_NAME"],
        INIT_HP["ALGO"],
        pop,
        sum_scores=False,
        INIT_HP=INIT_HP,
        MUT_P=MUTATION_PARAMS,
        swap_channels=INIT_HP["CHANNELS_LAST"],
        max_steps=INIT_HP["MAX_STEPS"],
        evo_steps=INIT_HP["EVO_STEPS"],
        eval_steps=INIT_HP["EVAL_STEPS"],
        eval_loop=INIT_HP["EVAL_LOOP"],
        target=INIT_HP["TARGET_SCORE"],
        tournament=tournament,
        mutation=mutations,
        wb=INIT_HP["WANDB"],
        checkpoint_path="/home/adam/haskell/haskboard/python/runs/",
        checkpoint=1000,
    )


if __name__ == "__main__":
    with open("ippo.yaml") as file:
        config = yaml.safe_load(file)
    main(config["INIT_HP"], config["MUTATION_PARAMS"], config["NET_CONFIG"])
