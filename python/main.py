"""
IPPO training script for haskboard games using AgileRL 2.x.

Usage (after building the Haskell binary):
    cabal build NoMerci
    uv run python main.py --binary $(cabal exec which NoMerci)

Docs:
    https://docs.agilerl.com/en/latest/api/algorithms/ippo.html
"""

from __future__ import annotations

import argparse

import gymnasium
import numpy as np
from agilerl.algorithms.ippo import IPPO

from haskboard_env import HaskboardEnv

# ---------------------------------------------------------------------------
# Observation flattening
# Dict + Sequence obs spaces → flat float32 vector suitable for MLP networks.
# ---------------------------------------------------------------------------

def _space_flat_dim(space: gymnasium.Space, max_seq_len: int) -> int:
    if isinstance(space, gymnasium.spaces.Dict):
        return sum(_space_flat_dim(s, max_seq_len) for s in space.spaces.values())
    if isinstance(space, gymnasium.spaces.Sequence):
        return max_seq_len * _space_flat_dim(space.feature_space, max_seq_len)
    from gymnasium.spaces.utils import flatdim
    return flatdim(space)


def flatten_obs(obs, space: gymnasium.Space, max_seq_len: int) -> np.ndarray:
    """Flatten a nested Dict/Sequence observation to a 1-D float32 array.

    - Dict sub-spaces are concatenated in insertion order.
    - Sequence sub-spaces are zero-padded / truncated to *max_seq_len* elements.
    - None (hidden location) becomes a zero vector matching the sub-space dim.
    """
    if obs is None:
        return np.zeros(_space_flat_dim(space, max_seq_len), dtype=np.float32)

    if isinstance(space, gymnasium.spaces.Dict):
        return np.concatenate(
            [flatten_obs(obs[k], s, max_seq_len) for k, s in space.spaces.items()]
        ).astype(np.float32)

    if isinstance(space, gymnasium.spaces.Sequence):
        inner_dim = _space_flat_dim(space.feature_space, max_seq_len)
        out = np.zeros(max_seq_len * inner_dim, dtype=np.float32)
        for i, item in enumerate(list(obs)[:max_seq_len]):
            out[i * inner_dim : (i + 1) * inner_dim] = flatten_obs(
                item, space.feature_space, max_seq_len
            )
        return out

    from gymnasium.spaces.utils import flatten as gym_flatten
    return gym_flatten(space, obs).astype(np.float32)


# ---------------------------------------------------------------------------
# Training
# ---------------------------------------------------------------------------

def train(
    binary: str,
    n_episodes: int = 1000,
    max_steps: int = 500,
    device: str = "cpu",
    max_seq_len: int = 64,
) -> None:
    env = HaskboardEnv(binary)

    native_obs_space = env.observation_space(env.possible_agents[0])
    native_act_space = env.action_space(env.possible_agents[0])
    obs_dim = _space_flat_dim(native_obs_space, max_seq_len)

    # IPPO needs flat Box obs spaces for its default MLP networks
    flat_obs_space = gymnasium.spaces.Box(
        low=-np.inf, high=np.inf, shape=(obs_dim,), dtype=np.float32
    )

    print(f"Agents:   {env.possible_agents}")
    print(f"Obs dim:  {obs_dim}  (flattened)")
    print(f"Act dim:  {native_act_space.n}")  # type: ignore[attr-defined]

    agent = IPPO(
        observation_spaces=[flat_obs_space] * len(env.possible_agents),
        action_spaces=[native_act_space] * len(env.possible_agents),
        agent_ids=env.possible_agents,
        device=device,
    )
    learn_step: int = agent.learn_step

    def flat(obs) -> np.ndarray:
        return flatten_obs(obs, native_obs_space, max_seq_len)

    # Per-step rollout buffers — each list will contain one entry per step.
    # IPPO.learn() expects a tuple of 8 dicts:
    #   (states, actions, log_probs, rewards, dones, values, next_states, next_dones)
    # where each dict maps agent_id → np.ndarray of shape [T, ...].
    buf_states:    list[dict[str, np.ndarray]] = []
    buf_actions:   list[dict[str, np.ndarray]] = []
    buf_log_probs: list[dict[str, np.ndarray]] = []
    buf_rewards:   list[dict[str, np.ndarray]] = []
    buf_dones:     list[dict[str, np.ndarray]] = []
    buf_values:    list[dict[str, np.ndarray]] = []

    def flush(final_states: dict[str, np.ndarray], final_done: bool) -> None:
        """Stack buffers and call agent.learn()."""
        T = len(buf_states)
        if T == 0:
            return

        def col(buf: list[dict], a: str) -> np.ndarray:
            return np.stack([buf[t][a] for t in range(T)])

        ids = agent.agent_ids
        experiences = (
            {a: col(buf_states,    a)           for a in ids},
            {a: col(buf_actions,   a).squeeze(-1) for a in ids},  # [T]
            {a: col(buf_log_probs, a).squeeze(-1) for a in ids},  # [T]
            {a: col(buf_rewards,   a)           for a in ids},
            {a: col(buf_dones,     a)           for a in ids},
            {a: col(buf_values,    a).squeeze(-1) for a in ids},  # [T]
            {a: final_states[a]                 for a in ids},
            {a: np.array([float(final_done)])   for a in ids},
        )
        agent.learn(experiences)
        for buf in (buf_states, buf_actions, buf_log_probs,
                    buf_rewards, buf_dones, buf_values):
            buf.clear()

    ep_rewards: list[float] = []
    steps_since_flush = 0
    states: dict[str, np.ndarray] = {a: np.zeros(obs_dim, dtype=np.float32) for a in env.possible_agents}
    final_done = False

    for episode in range(1, n_episodes + 1):
        observations, _ = env.reset()
        states = {a: flat(observations[a]) for a in env.possible_agents}
        ep_reward = {a: 0.0 for a in env.possible_agents}

        step = 0
        final_done = False
        while env.agents and step < max_steps:
            current = env.agent_selection

            obs_dict = {a: states[a] for a in env.possible_agents}
            actions_dict, log_probs_dict, _, values_dict = agent.get_action(obs_dict)

            # Only current agent acts; others get zero reward this step
            chosen_action = int(actions_dict[current][0])
            env.step(chosen_action)
            obs, reward, terminated, truncated, _ = env.last()

            ep_reward[current] += reward
            final_done = terminated or truncated
            states[current] = flat(obs)

            # Record step for all agents; non-acting agents get zero reward
            buf_states.append(dict(obs_dict))
            buf_actions.append({a: actions_dict[a] for a in env.possible_agents})
            buf_log_probs.append({a: log_probs_dict[a] for a in env.possible_agents})
            buf_rewards.append({a: np.array([reward if a == current else 0.0])
                                for a in env.possible_agents})
            buf_dones.append({a: np.array([float(terminated)])
                              for a in env.possible_agents})
            buf_values.append({a: values_dict[a] for a in env.possible_agents})

            steps_since_flush += 1
            if steps_since_flush >= learn_step:
                flush(states, final_done)
                steps_since_flush = 0

            step += 1
            if final_done:
                break

        ep_rewards.append(sum(ep_reward.values()))
        if episode % 50 == 0:
            mean_r = float(np.mean(ep_rewards[-50:]))
            print(f"Episode {episode:>5} | mean reward (last 50): {mean_r:+.3f}")

    # Final flush for any remaining experience
    flush(states, final_done)
    env.close()
    print("Training complete.")


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(description="Train IPPO on a haskboard game")
    parser.add_argument(
        "--binary",
        required=True,
        help="Path to compiled haskboard executable (must support --stdio)",
    )
    parser.add_argument("--episodes", type=int, default=1000)
    parser.add_argument("--max-steps", type=int, default=500)
    parser.add_argument("--device", default="cpu", choices=["cpu", "cuda", "mps"])
    parser.add_argument(
        "--max-seq-len",
        type=int,
        default=64,
        help="Pad length for variable-length Sequence obs (Deck locations)",
    )
    args = parser.parse_args()

    train(
        binary=args.binary,
        n_episodes=args.episodes,
        max_steps=args.max_steps,
        device=args.device,
        max_seq_len=args.max_seq_len,
    )


if __name__ == "__main__":
    main()
