import numpy as np
from agilerl.wrappers.agent import AsyncAgentsWrapper


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
