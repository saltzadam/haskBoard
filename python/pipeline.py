"""Training pipeline orchestrator for haskboard.

Runs the full collect -> BC train -> RL train pipeline as a single command,
with stage skipping when outputs already exist.

Usage:
    uv run --project python python/pipeline.py --name my_run --players 4
    uv run --project python python/pipeline.py --name my_run --force  # re-run all
"""

from __future__ import annotations

import argparse
import os
import subprocess
import sys
from pathlib import Path


def find_binary(binary_arg: str | None) -> str:
    """Return binary path, auto-detecting via cabal if not provided."""
    if binary_arg:
        return binary_arg
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
    except (subprocess.CalledProcessError, FileNotFoundError):
        pass
    raise FileNotFoundError(
        "Could not find NoMerci binary. Build with 'cabal build NoMerci' "
        "or pass --binary explicitly."
    )


def header(stage: str, status: str) -> None:
    print(f"\n{'='*60}")
    print(f"  {stage}  [{status}]")
    print(f"{'='*60}\n")


def run_stage(cmd: list[str]) -> None:
    """Run a subprocess, inheriting stdout/stderr. Aborts on failure."""
    result = subprocess.run(cmd)
    if result.returncode != 0:
        print(f"\nERROR: command failed (exit {result.returncode}):", file=sys.stderr)
        print(f"  {' '.join(cmd)}", file=sys.stderr)
        sys.exit(1)


def main() -> None:
    parser = argparse.ArgumentParser(description="haskboard training pipeline orchestrator")
    parser.add_argument("--name", required=True, help="Run name -> python/runs/<name>/")
    parser.add_argument("--players", type=int, default=3, help="Number of players, 3-5 (default: 3)")
    parser.add_argument("--collect-games", type=int, default=5000, help="Games for BC collection (default: 5000)")
    parser.add_argument("--bc-epochs", type=int, default=20, help="BC training epochs (default: 20)")
    parser.add_argument("--train-steps", type=int, default=1000, help="RL training iterations (default: 1000)")
    parser.add_argument("--force", action="store_true", help="Re-run all stages even if outputs exist")
    parser.add_argument("--binary", default=None, help="Path to Haskell binary (auto-detected)")
    args = parser.parse_args()

    binary = find_binary(args.binary)
    run_dir = Path(f"runs/{args.name}")

    bc_data_dir = run_dir / "bc_data"
    bc_ckpt_dir = run_dir / "bc_checkpoint"
    rl_ckpt_dir = run_dir / "rllib_checkpoints"

    summary: list[str] = []

    print(f"Pipeline: {args.name}")
    print(f"  binary:  {binary}")
    print(f"  players: {args.players}")
    print(f"  stages:  collect({args.collect_games}) -> bc({args.bc_epochs}ep) -> rl({args.train_steps}it)")

    # ── Stage 1: Collect BC data ──────────────────────────────────
    sentinel_1 = bc_data_dir / "init_msg.json"
    if sentinel_1.exists() and not args.force:
        header("Stage 1: Collect BC data", "SKIP")
        print(f"  Output exists: {sentinel_1}")
        summary.append("Stage 1 (collect): skipped")
    else:
        header("Stage 1: Collect BC data", "RUN")
        cmd = [
            "uv", "run", "--project", "python",
            "python", "python/collect_rllib.py",
            "--name", args.name,
            "--games", str(args.collect_games),
            "--binary", binary,
        ]
        if args.players != 3:
            cmd += ["--players", str(args.players)]
        run_stage(cmd)
        summary.append(f"Stage 1 (collect): completed ({args.collect_games} games)")

    # ── Stage 2: Train BC ─────────────────────────────────────────
    sentinel_2 = bc_ckpt_dir / "player_0" / "module_state.pkl"
    if sentinel_2.exists() and not args.force:
        header("Stage 2: Train BC", "SKIP")
        print(f"  Output exists: {sentinel_2}")
        summary.append("Stage 2 (bc): skipped")
    else:
        header("Stage 2: Train BC", "RUN")
        cmd = [
            "uv", "run", "--project", "python",
            "python", "python/train_bc.py",
            "--name", args.name,
            "--epochs", str(args.bc_epochs),
        ]
        run_stage(cmd)
        summary.append(f"Stage 2 (bc): completed ({args.bc_epochs} epochs)")

    # ── Stage 3: Train RL (PPO with BC warm-start) ────────────────
    has_rl_ckpts = rl_ckpt_dir.exists() and any(
        d.name.startswith("checkpoint_") for d in rl_ckpt_dir.iterdir()
    ) if rl_ckpt_dir.exists() else False
    if has_rl_ckpts and not args.force:
        header("Stage 3: Train RL (PPO)", "SKIP")
        print(f"  Checkpoints exist in: {rl_ckpt_dir}")
        summary.append("Stage 3 (rl): skipped")
    else:
        header("Stage 3: Train RL (PPO)", "RUN")
        cmd = [
            "uv", "run", "--project", "python",
            "python", "python/train_rllib.py",
            "--name", args.name,
            "--num-players", str(args.players),
            "--train-steps", str(args.train_steps),
            "--bc-checkpoint", str(bc_ckpt_dir),
            "--binary", binary,
        ]
        run_stage(cmd)
        summary.append(f"Stage 3 (rl): completed ({args.train_steps} iterations)")

    # ── Summary ───────────────────────────────────────────────────
    print(f"\n{'='*60}")
    print("  Pipeline complete")
    print(f"{'='*60}")
    for line in summary:
        print(f"  {line}")
    print(f"\n  Output: {run_dir.resolve()}")


if __name__ == "__main__":
    main()
