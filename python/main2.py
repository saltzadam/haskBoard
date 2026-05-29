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
import csv
import sys
import time
from pathlib import Path

import gymnasium
import numpy as np
from agilerl.algorithms.ippo import IPPO
from agilerl.algorithms.core.registry import HyperparameterConfig, RLParameter

from torch.utils.tensorboard import SummaryWriter

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
    log_dir: str = "runs",
    checkpoint_interval: int = 100,
    resume: str | None = None,
    shared: bool = True
) -> None:
    run_dir = Path(log_dir) / time.strftime("%Y%m%d_%H%M%S")
    ckpt_dir = run_dir / "checkpoints"
    ckpt_dir.mkdir(parents=True, exist_ok=True)
    writer = SummaryWriter(log_dir=str(run_dir))

    metrics_file = (run_dir / "metrics.csv").open("w", newline="")
    metrics_writer = csv.writer(metrics_file)
    metrics_writer.writerow(["episode", "steps", "total_reward", "mean_reward_50"])
    print(f"Logging to {run_dir}")

    env = HaskboardEnv(binary,shared=shared)

    native_obs_space = env.observation_space(env.possible_agents[0])
    native_act_space = env.action_space(env.possible_agents[0])
    obs_dim = _space_flat_dim(native_obs_space, max_seq_len)

    # IPPO needs flat Box obs spaces for its default MLP networks
    flat_obs_space = gymnasium.spaces.Box(
        low=-np.inf, high=np.inf, shape=(obs_dim,), dtype=np.float32
    )

    hp_config = HyperparameterConfig(
     lr=RLParameter(min=1e-4, max=1e-2),
        batch_size=RLParameter(min=32, max=256),
        learn_step=RLParameter(min=1, max=10, grow_factor=1.5, shrink_factor=0.75),
            )

    print(f"Agents:   {env.possible_agents}")
    print(f"Obs dim:  {obs_dim}  (flattened)")
    print(f"Act dim:  {native_act_space.n}")  # type: ignore[attr-defined]

    if resume:
        agent = IPPO.load(resume, device=device)
        print(f"Resumed from {resume}")
    else:
        agent = IPPO(
            observation_spaces=[flat_obs_space] * len(env.possible_agents),
            action_spaces=[native_act_space] * len(env.possible_agents),
            agent_ids=env.possible_agents,
            device=device,
            # batch_size=128,
            hp_config=hp_config
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
    best_mean_reward = float("-inf")

    for episode in range(1, n_episodes + 1):
        observations, _ = env.reset()
        states = {a: flat(observations[a]) for a in env.possible_agents}
        ep_reward = {a: 0.0 for a in env.possible_agents}

        step = 0
        final_done = False
        ep_start = time.monotonic()
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

        ep_duration = time.monotonic() - ep_start
        total_reward = sum(ep_reward.values())
        ep_rewards.append(total_reward)
        mean_r = float(np.mean(ep_rewards[-50:]))
        steps_per_sec = step / ep_duration if ep_duration > 0 else 0.0

        metrics_writer.writerow([episode, step, f"{total_reward:.3f}", f"{mean_r:.3f}"])
        metrics_file.flush()
        writer.add_scalar("reward/episode", total_reward, episode)
        writer.add_scalar("reward/mean50", mean_r, episode)
        writer.add_scalar("perf/steps_per_episode", step, episode)
        writer.add_scalar("perf/steps_per_sec", steps_per_sec, episode)

        if episode % 1 == 0:
            print(
                f"Episode {episode:>5} | steps {step:>4} | "
                f"reward {total_reward:+.2f} | mean50 {mean_r:+.3f} | "
                f"{steps_per_sec:.1f} steps/s"
            )

        # Periodic checkpoint
        if episode % checkpoint_interval == 0:
            ckpt_path = str(ckpt_dir / f"ep{episode}.pt")
            agent.save_checkpoint(ckpt_path)
            print(f"  → checkpoint saved: {ckpt_path}")

        # Best checkpoint
        if mean_r > best_mean_reward and episode >= 50:
            best_mean_reward = mean_r
            agent.save_checkpoint(str(run_dir / "best.pt"))

    # Final flush for any remaining experience
    flush(states, final_done)
    agent.save_checkpoint(str(run_dir / "final.pt"))
    writer.close()
    metrics_file.close()
    env.close()
    print(f"Training complete. Best mean reward: {best_mean_reward:+.3f}")
    print(f"Artifacts in {run_dir}")


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
    parser.add_argument(
        "--log-dir",
        default="runs",
        help="Directory for metrics CSV and checkpoints (default: runs/)",
    )
    parser.add_argument(
        "--checkpoint-interval",
        type=int,
        default=100,
        help="Save a checkpoint every N episodes (default: 100)",
    )
    parser.add_argument(
        "--resume",
        default=None,
        help="Path to a checkpoint .pt file to resume training from",
    )
    parser.add_argument(
            '--independent',
            action='store_true',
            default=False,
            help="Separate parameters in training"
            )
    args = parser.parse_args()

    train(
        binary=args.binary,
        n_episodes=args.episodes,
        max_steps=args.max_steps,
        device=args.device,
        max_seq_len=args.max_seq_len,
        log_dir=args.log_dir,
        checkpoint_interval=args.checkpoint_interval,
        resume=args.resume,
        shared= not(args.independent)
    )


if __name__ == "__main__":
    sys.stdout.reconfigure(line_buffering=True)  # type: ignore[union-attr]
    main()
