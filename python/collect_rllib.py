"""Behavioral cloning data collector for RLlib-based haskboard training.

Spawns the Haskell NoMerci binary with ``--collect``, reads the one-way JSON
stream, and writes per-player JSONL files compatible with RLlib's offline
data format.  Only expert (Hint) actions are recorded.

Usage:
    uv run --project python python python/collect_rllib.py --games 5000 --out python/bc_data/
"""

from __future__ import annotations

import argparse
import os
import subprocess
import sys
from pathlib import Path
from typing import Any

import gymnasium
import numpy as np
import orjson

from haskboard_aec_env import _build_space, _obs_to_numpy, _zeros


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
    except (subprocess.CalledProcessError, FileNotFoundError):
        pass
    raise FileNotFoundError(
        "Could not find NoMerci binary. Build with 'cabal build NoMerci' "
        "or pass --binary explicitly."
    )


def _numpy_to_json(obs: Any) -> Any:
    """Convert numpy observation to JSON-serializable form."""
    if isinstance(obs, dict):
        return {k: _numpy_to_json(v) for k, v in obs.items()}
    if isinstance(obs, np.ndarray):
        return obs.tolist()
    if isinstance(obs, (np.integer, np.floating)):
        return obs.item()
    return obs


def _build_action_mask(legal: list[int], n_actions: int) -> list[float]:
    mask = [0.0] * n_actions
    for i in legal:
        mask[i] = 1.0
    return mask


def _wrap_obs(game_obs: Any, legal: list[int], n_actions: int) -> dict:
    """Wrap game observation + action mask into the format HaskboardRLModule expects."""
    return {
        "observations": _numpy_to_json(game_obs),
        "action_mask": _build_action_mask(legal, n_actions),
    }


def collect(num_games: int, output_dir: str, binary_path: str) -> None:
    out = Path(output_dir)
    out.mkdir(parents=True, exist_ok=True)

    print(f"Spawning {binary_path} --collect ...", file=sys.stderr)
    proc = subprocess.Popen(
        [binary_path, "--collect"],
        stdout=subprocess.PIPE,
        stdin=subprocess.DEVNULL,
    )

    try:
        # 1. Read InitMsg
        init_line = proc.stdout.readline()
        if not init_line:
            print("ERROR: process closed stdout before InitMsg", file=sys.stderr)
            sys.exit(1)

        init_msg: dict = orjson.loads(init_line)
        agent_ids: list[int] = init_msg["agents"]
        n_agents = len(agent_ids)

        # Save init_msg for train_bc.py to reconstruct spaces
        with open(out / "init_msg.json", "wb") as f:
            f.write(orjson.dumps(init_msg, option=orjson.OPT_INDENT_2))

        # Build per-agent observation spaces
        obs_spaces: dict[int, gymnasium.Space] = {
            int(k): _build_space(v)
            for k, v in init_msg["observationSpaces"].items()
        }
        act_space = _build_space(init_msg["actionSpace"])
        n_actions: int = act_space.n

        print(f"Agents: {agent_ids}, actions: {n_actions}", file=sys.stderr)

        # Per-player output files
        writers = {
            aid: open(out / f"bc_data_player_{aid}.jsonl", "wb")
            for aid in agent_ids
        }

        # Per-agent buffering for new_obs tracking
        prev_step: dict[int, dict | None] = {aid: None for aid in agent_ids}

        game_count = 0
        terminal_count = 0
        eps_id = 0
        step_counters: dict[int, int] = {aid: 0 for aid in agent_ids}
        sample_counts: dict[int, int] = {aid: 0 for aid in agent_ids}

        print(f"Collecting {num_games} games ...", file=sys.stderr)

        while game_count < num_games:
            line = proc.stdout.readline()
            if not line:
                print(
                    f"WARNING: process closed after {game_count} games.",
                    file=sys.stderr,
                )
                break

            msg: dict = orjson.loads(line)
            aid: int = msg["agent"]

            if msg["msgType"] == "terminal":
                # Finalize buffered step with zero new_obs
                if prev_step[aid] is not None:
                    zero_obs = _zeros(obs_spaces[aid])
                    prev_step[aid]["new_obs"] = _wrap_obs(zero_obs, [], n_actions)
                    prev_step[aid]["terminateds"] = True
                    writers[aid].write(orjson.dumps(prev_step[aid]) + b"\n")
                    sample_counts[aid] += 1
                    prev_step[aid] = None

                terminal_count += 1
                if terminal_count == n_agents:
                    game_count += 1
                    terminal_count = 0
                    eps_id += 1
                    step_counters = {a: 0 for a in agent_ids}
                    if game_count % 500 == 0:
                        total = sum(sample_counts.values())
                        print(
                            f"  {game_count}/{num_games} games "
                            f"({total} hint samples)",
                            file=sys.stderr,
                        )
                continue

            # Normal step
            game_obs = _obs_to_numpy(msg["observation"], obs_spaces[aid])
            legal: list[int] = msg["legalActions"]
            wrapped = _wrap_obs(game_obs, legal, n_actions)

            # Finalize previous buffered step for this agent
            if prev_step[aid] is not None:
                prev_step[aid]["new_obs"] = wrapped
                writers[aid].write(orjson.dumps(prev_step[aid]) + b"\n")
                sample_counts[aid] += 1
                prev_step[aid] = None

            # Only buffer Hint-sourced steps
            if msg.get("actionSource") == "Hint" and msg.get("hintAction") is not None:
                prev_step[aid] = {
                    "obs": wrapped,
                    "actions": msg["hintAction"],
                    "rewards": 0.0,
                    "terminateds": False,
                    "truncateds": False,
                    "eps_id": f"game_{eps_id}",
                    "t": step_counters[aid],
                }
            step_counters[aid] += 1

    finally:
        proc.terminate()
        for w in writers.values():
            w.close()

    # Print stats
    total = sum(sample_counts.values())
    print(f"\nCollection complete.", file=sys.stderr)
    print(f"Games collected: {game_count}", file=sys.stderr)
    print(f"Total hint samples: {total}", file=sys.stderr)
    for aid in agent_ids:
        print(f"  player_{aid}: {sample_counts[aid]} samples", file=sys.stderr)
    print(f"Output directory: {out}", file=sys.stderr)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Collect BC data from NoMerci for RLlib training."
    )
    parser.add_argument("--games", type=int, default=5000,
                        help="Number of games to collect (default: 5000)")
    parser.add_argument("--out", default="python/bc_data/",
                        help="Output directory (default: python/bc_data/)")
    parser.add_argument("--binary", default=None,
                        help="Path to Haskell binary (auto-detected if omitted)")
    args = parser.parse_args()
    binary = args.binary or find_binary()
    collect(num_games=args.games, output_dir=args.out, binary_path=binary)


if __name__ == "__main__":
    main()
