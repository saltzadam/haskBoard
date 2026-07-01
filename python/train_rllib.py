"""
PPO training script for haskboard games using RLlib's new API stack.

Usage:
    python train_rllib.py --binary /path/to/NoMerci --num-players 3
    python train_rllib.py  # auto-detects binary via cabal list-bin
"""

from __future__ import annotations

import argparse
import os
import pickle
import subprocess
import sys
import json
import time
from pathlib import Path
from typing import Any

import ray
from ray.rllib.algorithms.algorithm import Algorithm
from ray.rllib.algorithms.ppo import PPOConfig
from ray.rllib.core.rl_module.multi_rl_module import MultiRLModuleSpec
from ray.rllib.core.rl_module.rl_module import RLModuleSpec
from ray.rllib.env.wrappers.pettingzoo_env import PettingZooEnv
from ray.tune.registry import register_env

from torch.utils.tensorboard import SummaryWriter

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
    num_players = config.get("num_players")
    aec_env = HaskboardAECEnv(binary_path=binary, extra_args=extra_args, num_players=num_players)
    return PettingZooEnv(aec_env)


def _resolve_name(name: str | None) -> str:
    """Return the run name, generating a timestamp if not provided."""
    if name is not None:
        return name
    from datetime import datetime
    return datetime.now().strftime("%Y-%m-%d_%H%M%S")


def main() -> None:
    parser = argparse.ArgumentParser(description="Train PPO on haskboard via RLlib")
    parser.add_argument("--name", type=str, default=None,
                        help="Run name; checkpoints go to python/runs/<name>/rllib_checkpoints/ (auto-timestamp if omitted)")
    parser.add_argument("--binary", type=str, default=None,
                        help="Path to Haskell game binary (auto-detected if omitted)")
    parser.add_argument("--num-players", type=int, default=3,
                        help="Number of players (default: 3)")
    parser.add_argument("--num-env-runners", type=int, default=6,
                        help="Number of parallel env runners (default: 6)")
    parser.add_argument("--train-steps", type=int, default=1000,
                        help="Number of training iterations (default: 1000)")
    parser.add_argument("--bc-checkpoint", type=str, default=None,
                        help="Path to BC checkpoint dir to warm-start from")
    parser.add_argument("--force", action="store_true",
                        help="Force restart from scratch, ignoring existing checkpoints")
    args = parser.parse_args()

    name = _resolve_name(args.name)
    print(f"Run name: {name}")

    binary_path = args.binary or find_binary()
    num_players = args.num_players

    ray_tmp = Path(__file__).parent / "ray_tmp"
    ray_tmp.mkdir(exist_ok=True)
    ray.init(
        _temp_dir=str(ray_tmp),
        runtime_env={"working_dir": ".", "excludes": [".venv/", "runs/", "__pycache__/", "ray_tmp/", "*.npz", "bc_data/", "bc_checkpoint/"]},
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
                "num_players": num_players,
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
            lr=3e-5, # down one oom
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

    checkpoint_dir = str(Path(f"runs/{name}/rllib_checkpoints").resolve())
    os.makedirs(checkpoint_dir, exist_ok=True)
    progress_file = Path(f"runs/{name}/progress.json")

    # Try to resume from existing checkpoint
    start_step = 0
    algo = None

    if not args.force and progress_file.exists():
        with open(progress_file) as f:
            progress = json.load(f)
        last_step = progress.get("step", 0)
        ckpt_path = progress.get("checkpoint_path")
        if ckpt_path and Path(ckpt_path).exists():
            print(f"Resuming from step {last_step}: {ckpt_path}")
            algo = Algorithm.from_checkpoint(ckpt_path)
            start_step = last_step
        else:
            print(f"Warning: checkpoint not found at {ckpt_path}, starting fresh", file=sys.stderr)

    if algo is None:
        algo = config.build_algo()

        if args.bc_checkpoint:
            bc_path = Path(args.bc_checkpoint)
            print(f"Loading BC weights from {bc_path}...")
            for pname in policy_names:
                state_file = bc_path / pname / "module_state.pkl"
                if not state_file.exists():
                    print(f"WARNING: {state_file} not found, skipping {pname}", file=sys.stderr)
                    continue
                with open(state_file, "rb") as f:
                    state = pickle.load(f)
                algo.learner_group._learner.module[pname].set_state(state)
            algo.env_runner_group.sync_weights(
                from_worker_or_learner_group=algo.learner_group,
                inference_only=True,
            )
            print("BC weights loaded and synced to env runners.")

    tb_dir = str(Path(f"runs/{name}/tensorboard").resolve())
    writer = SummaryWriter(log_dir=tb_dir)

    total = args.train_steps
    remaining = total - start_step
    print(f"Training steps {start_step + 1}–{total} ({remaining} remaining) | {num_players} players | {args.num_env_runners} runners")

    train_start = time.time()
    for step in range(start_step + 1, total + 1):
        step_start = time.time()
        results = algo.train()
        step_duration = time.time() - step_start
        total_elapsed = time.time() - train_start

        # -- TensorBoard logging --
        env_runners = results.get("env_runners", {})

        # Per-policy reward
        policy_reward = env_runners.get("module_episode_returns_mean", {})
        for pn in policy_names:
            r = policy_reward.get(pn)
            if r is not None:
                writer.add_scalar(f"rllib/rewards/{pn}", r, step)

        # Global episode metrics
        for key, tag in [
            ("episode_return_mean", "rllib/episode_return_mean"),
            ("episode_len_mean", "rllib/episode_len_mean"),
            ("num_env_steps_sampled_lifetime", "rllib/num_env_steps_sampled_lifetime"),
        ]:
            val = env_runners.get(key)
            if val is not None:
                writer.add_scalar(tag, val, step)

        # Per-policy learner metrics
        learners = results.get("learners", {})
        for pn in policy_names:
            plearner = learners.get(pn, {})
            for key, suffix in [
                ("policy_loss", "policy_loss"),
                ("vf_loss", "vf_loss"),
                ("entropy", "entropy"),
                ("mean_kl_loss", "mean_kl_loss"),
                ("vf_explained_var", "vf_explained_var"),
            ]:
                val = plearner.get(key)
                if val is not None:
                    writer.add_scalar(f"rllib/learners/{pn}/{suffix}", val, step)

        # Timing
        writer.add_scalar("rllib/time/step_duration_s", step_duration, step)
        writer.add_scalar("rllib/time/total_elapsed_s", total_elapsed, step)

        # -- Console logging & checkpoints (every 10 steps) --
        if step % 10 == 0:
            remaining = total - step
            reward_parts = []
            for pn in policy_names:
                r = policy_reward.get(pn, float("nan"))
                reward_parts.append(f"{pn}={r:.4f}")
            reward_str = ", ".join(reward_parts) if reward_parts else "N/A"
            print(f"Step {step}/{total} ({remaining} remaining)  rewards=[{reward_str}]")
            save_result = algo.save(checkpoint_dir)
            print(f"  checkpoint: {save_result.checkpoint.path}")
            with open(progress_file, "w") as f:
                json.dump({"step": step, "checkpoint_path": str(save_result.checkpoint.path)}, f)

    writer.close()
    algo.stop()
    ray.shutdown()
    print(f"Training complete. Final checkpoint: {checkpoint_dir}")


if __name__ == "__main__":
    main()
