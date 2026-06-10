"""Behavioural-cloning data collector for haskboard games.

Spawns the Haskell NoMerci binary with ``--collect``, reads the one-way JSON
stream (no stdin required), applies the same observation transformations as
HaskboardEnv, and saves the dataset to a compressed ``.npz`` file.

Usage
-----
    uv run python collect_bc.py --games 5000 --out bc_data.npz
"""

from __future__ import annotations

import argparse
import subprocess
import sys
from typing import Any

import numpy as np
import orjson

import gymnasium

from haskboard_env import (
    RunningStats,
    _boxify_obs,
    _boxify_space,
    _build_space,
    _extract_norm_hints,
    _obs_to_numpy,
)

# ---------------------------------------------------------------------------
# Binary path
# ---------------------------------------------------------------------------

BINARY_PATH = (
    "/home/adam/haskell/haskboard/dist-newstyle/build/x86_64-linux/ghc-9.10.1"
    "/NoMerci-0.1.0.0/x/NoMerci/build/NoMerci/NoMerci"
)


# ---------------------------------------------------------------------------
# Normalization (mirrors HaskboardEnv._normalize_obs exactly)
# ---------------------------------------------------------------------------


def _normalize_obs(
    obs: Any,
    agent_id: int,
    norm_hints: dict[int, dict[str, str]],
    minmax_bounds: dict[int, dict[str, tuple[np.ndarray, np.ndarray]]],
    running_stats: dict[int, dict[str, RunningStats]],
) -> Any:
    """Apply per-subspace normalization to a boxified observation dict."""
    if not isinstance(obs, dict):
        return obs
    hints = norm_hints.get(agent_id, {})
    result: dict[str, Any] = {}
    for k, v in obs.items():
        hint = hints.get(k, "none")
        if hint == "minmax":
            lo, hi = minmax_bounds[agent_id][k]
            denom = hi - lo
            denom = np.where(denom == 0, 1.0, denom)
            result[k] = (v - lo) / denom
        elif hint == "standardize":
            stats = running_stats[agent_id].get(k)
            if stats is not None:
                stats.update(v)
                result[k] = stats.normalize(v)
            else:
                result[k] = v
        else:
            result[k] = v
    return result


# ---------------------------------------------------------------------------
# Main collection loop
# ---------------------------------------------------------------------------


def collect(num_games: int, output_path: str) -> None:
    print(f"Spawning {BINARY_PATH} --collect ...")
    proc = subprocess.Popen(
        [BINARY_PATH, "--collect"],
        stdout=subprocess.PIPE,
        stdin=subprocess.DEVNULL,
    )

    try:
        # ------------------------------------------------------------------
        # 1. Read InitMsg
        # ------------------------------------------------------------------
        init_line = proc.stdout.readline()
        if not init_line:
            print("ERROR: process closed stdout before sending InitMsg", file=sys.stderr)
            sys.exit(1)

        init_msg: dict = orjson.loads(init_line)
        agent_ids: list[int] = init_msg["agents"]
        num_agents = len(agent_ids)
        obs_spaces_raw_spec: dict[str, dict] = init_msg["observationSpaces"]

        print(f"Agents: {agent_ids}")

        # ------------------------------------------------------------------
        # 2. Build raw spaces (keyed by int agent id)
        # ------------------------------------------------------------------
        raw_spaces: dict[int, Any] = {
            int(i): _build_space(s) for i, s in obs_spaces_raw_spec.items()
        }

        # ------------------------------------------------------------------
        # 3. Normalization setup (mirrors HaskboardEnv.__init__)
        # ------------------------------------------------------------------
        norm_hints: dict[int, dict[str, str]] = {
            int(i): _extract_norm_hints(s) for i, s in obs_spaces_raw_spec.items()
        }

        # Boxified spaces with original bounds — needed for minmax reference ranges
        orig_boxified: dict[int, Any] = {
            aid: _boxify_space(raw_spaces[aid]) for aid in agent_ids
        }

        minmax_bounds: dict[int, dict[str, tuple[np.ndarray, np.ndarray]]] = {}
        running_stats: dict[int, dict[str, RunningStats]] = {}

        for aid in agent_ids:
            minmax_bounds[aid] = {}
            running_stats[aid] = {}
            hints = norm_hints.get(aid, {})
            box_space = orig_boxified[aid]
            if isinstance(box_space, gymnasium.spaces.Dict):
                for k, sub in box_space.spaces.items():
                    hint = hints.get(k, "none")
                    if hint == "minmax" and isinstance(sub, gymnasium.spaces.Box):
                        minmax_bounds[aid][k] = (sub.low.copy(), sub.high.copy())
                    elif hint == "standardize" and isinstance(sub, gymnasium.spaces.Box):
                        running_stats[aid][k] = RunningStats(sub.shape)

        # ------------------------------------------------------------------
        # 4. Determine obs keys from first agent's boxified space
        # ------------------------------------------------------------------
        first_aid = agent_ids[0]
        first_box = orig_boxified[first_aid]
        if isinstance(first_box, gymnasium.spaces.Dict):
            obs_keys: list[str] = list(first_box.spaces.keys())
        else:
            obs_keys = ["obs"]

        # ------------------------------------------------------------------
        # 5. Collection loop
        # ------------------------------------------------------------------
        obs_samples: list[dict[str, np.ndarray]] = []
        action_samples: list[int] = []
        source_samples: list[str] = []

        game_count = 0
        terminal_count = 0

        print(f"Collecting {num_games} games ...")

        while game_count < num_games:
            line = proc.stdout.readline()
            if not line:
                print(
                    f"WARNING: process closed stdout after {game_count} games.",
                    file=sys.stderr,
                )
                break

            msg: dict = orjson.loads(line)

            if msg["msgType"] == "terminal":
                terminal_count += 1
                if terminal_count == num_agents:
                    game_count += 1
                    terminal_count = 0
                    if game_count % 500 == 0:
                        print(
                            f"  {game_count}/{num_games} games collected "
                            f"({len(obs_samples)} samples so far)"
                        )
                continue

            # Normal step: record (obs, hintAction)
            agent_id: int = msg["agent"]
            raw_obs = _obs_to_numpy(msg["observation"], raw_spaces[agent_id])
            boxified = _boxify_obs(raw_obs, raw_spaces[agent_id])
            normalized = _normalize_obs(
                boxified, agent_id, norm_hints, minmax_bounds, running_stats
            )
            obs_samples.append(normalized)
            action_samples.append(int(msg["hintAction"]))
            source_samples.append(msg.get("actionSource", "Hint"))

    finally:
        proc.terminate()

    # ------------------------------------------------------------------
    # 6. Save to .npz
    # ------------------------------------------------------------------
    if not obs_samples:
        print("No samples collected — nothing to save.", file=sys.stderr)
        sys.exit(1)

    arrays: dict[str, np.ndarray] = {}

    if isinstance(obs_samples[0], dict):
        for key in obs_keys:
            arrays[f"obs_{key}"] = np.stack([s[key] for s in obs_samples])
    else:
        # Flat (non-Dict) obs — should not happen with current NoMerci spaces
        arrays["obs"] = np.stack(obs_samples)

    arrays["actions"] = np.array(action_samples, dtype=np.int64)

    source_map = {"Hint": 0, "Random": 1, "Agent": 2, "Human": 3}
    arrays["action_source"] = np.array(
        [source_map.get(s, 2) for s in source_samples], dtype=np.int8
    )

    np.savez_compressed(output_path, **arrays)

    # ------------------------------------------------------------------
    # 7. Print stats
    # ------------------------------------------------------------------
    total_samples = len(action_samples)
    print(f"\nDataset saved to: {output_path}")
    print(f"Total samples   : {total_samples}")
    print("Shapes per key  :")
    for k, arr in arrays.items():
        print(f"  {k:30s} {arr.shape}  dtype={arr.dtype}")

    actions_arr = arrays["actions"]
    unique, counts = np.unique(actions_arr, return_counts=True)
    print("Action distribution:")
    for a, c in zip(unique, counts):
        print(f"  action {a:3d}: {c:6d}  ({100.0 * c / total_samples:.1f}%)")

    source_names = {0: "Hint", 1: "Random", 2: "Agent", 3: "Human"}
    source_arr = arrays["action_source"]
    src_unique, src_counts = np.unique(source_arr, return_counts=True)
    print("Source distribution:")
    for s, c in zip(src_unique, src_counts):
        print(f"  {source_names.get(int(s), '?'):8s}: {c:6d}  ({100.0 * c / total_samples:.1f}%)")


# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Collect behavioural-cloning data from the NoMerci Haskell binary."
    )
    parser.add_argument(
        "--games",
        type=int,
        default=5000,
        help="Number of complete games to collect (default: 5000).",
    )
    parser.add_argument(
        "--out",
        default="bc_data.npz",
        help="Output .npz file path (default: bc_data.npz).",
    )
    args = parser.parse_args()
    collect(num_games=args.games, output_path=args.out)


if __name__ == "__main__":
    main()
