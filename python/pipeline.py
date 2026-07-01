"""Training pipeline orchestrator for haskboard.

Runs the full collect -> BC train -> RL train pipeline as a single command,
with stage skipping when outputs already exist.

Usage:
    uv run --project python python/pipeline.py --game NoMerci --min-players 3 --max-players 5
    uv run --project python python/pipeline.py --game NoMerci --force  # re-run all
    uv run --project python python/pipeline.py --game NoMerci --name custom_run
"""

from __future__ import annotations

import argparse
import os
import subprocess
import sys
from pathlib import Path


def find_binary(game: str) -> str:
    """Return binary path for a game, auto-detecting via cabal list-bin."""
    try:
        result = subprocess.run(
            ["cabal", "list-bin", game],
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
        f"Could not find {game} binary. Build with 'cabal build {game}' first."
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
    parser.add_argument("--game", required=True, help="Game executable name (e.g. NoMerci)")
    parser.add_argument("--name", default=None, help="Run name -> runs/<name>_<N>/ (default: {game}_default_<N>)")
    parser.add_argument("--min-players", type=int, default=3, help="Minimum number of players (default: 3)")
    parser.add_argument("--max-players", type=int, default=3, help="Maximum number of players (default: 3)")
    parser.add_argument("--collect-games", type=int, default=5000, help="Games for BC collection (default: 5000)")
    parser.add_argument("--bc-epochs", type=int, default=20, help="BC training epochs (default: 20)")
    parser.add_argument("--train-steps", type=int, default=1000, help="RL training iterations (default: 1000)")
    parser.add_argument("--num-env-runners", type=int, default=6, help="Parallel env runners for RL training (default: 6)")
    parser.add_argument("--force", action="store_true", help="Re-run all stages even if outputs exist")
    args = parser.parse_args()

    # Resolve sibling scripts relative to this file's directory
    script_dir = Path(__file__).resolve().parent
    collect_script = str(script_dir / "collect_rllib.py")
    bc_script = str(script_dir / "train_bc.py")
    rl_script = str(script_dir / "train_rllib.py")
    project_dir = str(script_dir)

    binary = find_binary(args.game)
    base_name = args.name or f"{args.game}_default"
    player_counts = range(args.min_players, args.max_players + 1)
    num_iterations = len(player_counts)

    summary: list[str] = []

    if args.min_players == args.max_players:
        players_label = str(args.min_players)
    else:
        players_label = f"{args.min_players}-{args.max_players}"

    print(f"Pipeline: {base_name}")
    print(f"  game:    {args.game}")
    print(f"  binary:  {binary}")
    print(f"  players: {players_label}")
    stages_desc = f"collect({args.collect_games}) -> bc({args.bc_epochs}ep) -> rl({args.train_steps}it)"
    if num_iterations > 1:
        stages_desc += f"  x{num_iterations} player counts"
    print(f"  stages:  {stages_desc}")

    for num_players in player_counts:
        name = f"{base_name}_{num_players}"
        run_dir = Path(f"runs/{name}")

        bc_data_dir = run_dir / "bc_data"
        bc_ckpt_dir = run_dir / "bc_checkpoint"
        rl_ckpt_dir = run_dir / "rllib_checkpoints"

        if num_iterations > 1:
            print(f"\n{'#'*60}")
            print(f"  {num_players} players — {name}")
            print(f"{'#'*60}")
            summary.append(f"--- {num_players} players ---")

        # ── Stage 1: Collect BC data ──────────────────────────────────
        sentinel_1 = bc_data_dir / "init_msg.json"
        if sentinel_1.exists() and not args.force:
            header("Stage 1: Collect BC data", "SKIP")
            print(f"  Output exists: {sentinel_1}")
            summary.append("Stage 1 (collect): skipped")
        else:
            header("Stage 1: Collect BC data", "RUN")
            cmd = [
                "uv", "run", "--project", project_dir,
                "python", collect_script,
                "--name", name,
                "--games", str(args.collect_games),
                "--binary", binary,
                "--players", str(num_players),
            ]
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
                "uv", "run", "--project", project_dir,
                "python", bc_script,
                "--name", name,
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
                "uv", "run", "--project", project_dir,
                "python", rl_script,
                "--name", name,
                "--num-players", str(num_players),
                "--train-steps", str(args.train_steps),
                "--num-env-runners", str(args.num_env_runners),
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
    print(f"\n  Output: runs/{base_name}_*/")


if __name__ == "__main__":
    main()
