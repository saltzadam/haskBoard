"""
PPO training script for haskboard games using RLlib's new API stack.

Usage:
    python train_rllib.py --binary /path/to/NoMerci --num-players 3
    python train_rllib.py  # auto-detects binary via cabal list-bin
"""

from __future__ import annotations

import argparse
import os
import subprocess
import sys
from pathlib import Path
from typing import Any

import ray
from ray import tune
from ray.rllib.algorithms.ppo import PPOConfig
from ray.rllib.core.rl_module.multi_rl_module import MultiRLModuleSpec
from ray.rllib.core.rl_module.rl_module import RLModuleSpec
from ray.rllib.env.wrappers.pettingzoo_env import PettingZooEnv
from ray.tune.registry import register_env

from haskboard_aec_env import HaskboardAECEnv
from haskboard_rl_module import HaskboardRLModule


def find_binary() -> str:
    """Locate the NoMerci binary using cabal list-bin."""
    try:
        result = subprocess.run(
            ["cabal", "list-bin", "NoMerci"],
            capture_output=True,
            text=True,
            check=True,
        )
        path = result.stdout.strip()
        if os.path.isfile(path):
            return path
        print(f"Warning: cabal list-bin returned {path!r} but file not found", file=sys.stderr)
    except (subprocess.CalledProcessError, FileNotFoundError) as e:
        print(f"Warning: cabal list-bin failed: {e}", file=sys.stderr)

    # Fallback: search common locations
    fallbacks = [
        os.path.expanduser("~/haskell/haskboard/dist-newstyle/build/x86_64-linux/ghc-9.10.1/NoMerci-0.1.0.0/x/NoMerci/build/NoMerci/NoMerci"),
    ]
    for fb in fallbacks:
        if os.path.isfile(fb):
            return fb
    raise FileNotFoundError(
        "Could not find NoMerci binary. Build with 'cabal build NoMerci' "
        "or pass --binary explicitly."
    )


def env_creator(config: dict[str, Any]) -> PettingZooEnv:
    """Factory for creating PettingZoo-wrapped haskboard AEC environments."""
    binary = config.get("binary_path", find_binary())
    extra_args = config.get("extra_args", [])
    aec_env = HaskboardAECEnv(binary_path=binary, extra_args=extra_args)
    return PettingZooEnv(aec_env)


def main() -> None:
    parser = argparse.ArgumentParser(description="Train PPO on haskboard via RLlib")
    parser.add_argument("--binary", type=str, default=None,
                        help="Path to Haskell game binary (auto-detected if omitted)")
    parser.add_argument("--num-players", type=int, default=3,
                        help="Number of players (default: 3)")
    parser.add_argument("--num-env-runners", type=int, default=4,
                        help="Number of parallel env runners (default: 4)")
    parser.add_argument("--train-steps", type=int, default=1000,
                        help="Number of training iterations (default: 1000)")
    args = parser.parse_args()

    binary_path = args.binary or find_binary()
    num_players = args.num_players

    ray_tmp = Path(__file__).parent / "ray_tmp"
    ray_tmp.mkdir(exist_ok=True)
    ray.init(
        _temp_dir=str(ray_tmp),
        runtime_env={"working_dir": ".", "excludes": [".venv/", "runs/", "__pycache__/", "ray_tmp/"]},
    )

    # Register the environment
    register_env("haskboard", env_creator)

    # Policy names: one per player (independent learning)
    policy_names = [f"player_{i}" for i in range(num_players)]

    def policy_mapping_fn(agent_id: str, episode: Any = None, worker: Any = None, **kwargs: Any) -> str:
        return agent_id

    # Build RLModule specs: one per policy, all using HaskboardRLModule
    module_specs = {
        name: RLModuleSpec(
            module_class=HaskboardRLModule,
            model_config={
                "hidden_dims": [256, 256],
                "trunk_dim": 128,
            },
        )
        for name in policy_names
    }

    config = (
        PPOConfig()
        .environment(
            env="haskboard",
            env_config={
                "binary_path": binary_path,
                "extra_args": [],
            },
        )
        .env_runners(
            num_env_runners=args.num_env_runners,
        )
        .multi_agent(
            policies=set(policy_names),
            policy_mapping_fn=policy_mapping_fn,
        )
        .rl_module(
            rl_module_spec=MultiRLModuleSpec(
                rl_module_specs=module_specs,
            ),
        )
        .training(
            lr=3e-4,
            gamma=0.99,
            lambda_=0.95,
            clip_param=0.2,
            entropy_coeff=0.01,
            vf_loss_coeff=0.5,
            train_batch_size_per_learner=4096,
            minibatch_size=256,
            num_epochs=4,
        )
    )

    algo = config.build_algo()

    checkpoint_dir = os.path.expanduser("~/haskell/haskboard/python/runs/rllib_checkpoints")
    os.makedirs(checkpoint_dir, exist_ok=True)

    print(f"Starting training for {args.train_steps} iterations...")
    print(f"Binary: {binary_path}")
    print(f"Players: {num_players}")
    print(f"Env runners: {args.num_env_runners}")

    for step in range(1, args.train_steps + 1):
        results = algo.train()

        if step % 10 == 0:
            # Print per-policy mean rewards
            reward_parts = []
            env_runners = results.get("env_runners", {})
            policy_reward = env_runners.get("module_episode_returns_mean", {})
            for name in policy_names:
                r = policy_reward.get(name, float("nan"))
                reward_parts.append(f"{name}={r:.4f}")
            reward_str = ", ".join(reward_parts) if reward_parts else "N/A"
            print(f"Step {step}: rewards=[{reward_str}]")

        if step % 10 == 0:
            save_path = algo.save(checkpoint_dir)
            print(f"Checkpoint saved: {save_path}")

    algo.stop()
    ray.shutdown()
    print("Training complete.")


if __name__ == "__main__":
    main()
