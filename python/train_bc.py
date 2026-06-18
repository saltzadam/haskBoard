"""Behavioral cloning trainer for haskboard using HaskboardRLModule.

Loads per-player JSONL data from collect_rllib.py, trains each player's
HaskboardRLModule with cross-entropy loss, and saves weights in RLlib
checkpoint format.

Usage:
    uv run --project python python python/train_bc.py \
        --data python/bc_data/ --epochs 20 --out python/bc_checkpoint/
"""

from __future__ import annotations

import argparse
import math
import pickle
import sys
from pathlib import Path
from typing import Any

import gymnasium
import numpy as np
import orjson
import torch
import torch.nn.functional as F
from ray.rllib.core.columns import Columns

from haskboard_aec_env import _build_space
from haskboard_rl_module import HaskboardRLModule

MODEL_CONFIG = {"hidden_dims": [256, 256], "trunk_dim": 128}


def _json_to_numpy(obs_json: Any, space: gymnasium.Space) -> Any:
    """Convert JSON-serialized obs back to numpy arrays matching space."""
    if obs_json is None:
        if isinstance(space, gymnasium.spaces.MultiBinary):
            return np.zeros(space.n, dtype=np.int8)
        elif isinstance(space, gymnasium.spaces.Box):
            return np.zeros(space.shape, dtype=np.float32)
        elif isinstance(space, gymnasium.spaces.Discrete):
            return np.int64(0)
        return obs_json
    if isinstance(space, gymnasium.spaces.Dict):
        return {k: _json_to_numpy(obs_json.get(k), s) for k, s in space.spaces.items()}
    elif isinstance(space, gymnasium.spaces.MultiBinary):
        return np.array(obs_json, dtype=np.int8)
    elif isinstance(space, gymnasium.spaces.Box):
        return np.array(obs_json, dtype=np.float32).reshape(space.shape)
    elif isinstance(space, gymnasium.spaces.Discrete):
        return np.int64(obs_json)
    elif isinstance(space, gymnasium.spaces.MultiDiscrete):
        return np.array(obs_json, dtype=np.int64)
    return obs_json


def _collate_obs(
    obs_list: list[dict[str, Any]],
    game_space: gymnasium.Space,
    n_actions: int,
) -> dict[str, Any]:
    """Stack a list of wrapped observations into a batched dict of tensors."""
    # Each obs_list item is {"observations": {...}, "action_mask": [...]}
    masks = torch.tensor(
        [row["action_mask"] for row in obs_list], dtype=torch.float32,
    )

    if isinstance(game_space, gymnasium.spaces.Dict):
        game_obs: dict[str, torch.Tensor] = {}
        sorted_keys = sorted(game_space.spaces.keys())
        for key in sorted_keys:
            sub = game_space.spaces[key]
            # Skip Discrete(1) dummies (matches HaskboardRLModule._encode)
            if isinstance(sub, gymnasium.spaces.Discrete) and sub.n == 1:
                continue
            vals = [
                _json_to_numpy(row["observations"][key], sub)
                for row in obs_list
            ]
            game_obs[key] = torch.tensor(np.array(vals), dtype=torch.float32)
        return {"observations": game_obs, "action_mask": masks}
    else:
        vals = [
            _json_to_numpy(row["observations"], game_space)
            for row in obs_list
        ]
        return {
            "observations": torch.tensor(np.array(vals), dtype=torch.float32),
            "action_mask": masks,
        }


def train_player(
    policy_name: str,
    data_path: Path,
    game_obs_space: gymnasium.Space,
    act_space: gymnasium.Space,
    n_actions: int,
    args: argparse.Namespace,
    device: torch.device,
) -> dict:
    """Train one player's module and return its state dict."""
    # Load data
    rows: list[dict] = []
    with open(data_path, "rb") as f:
        for line in f:
            rows.append(orjson.loads(line))

    if not rows:
        print(f"  WARNING: no data for {policy_name}, skipping", file=sys.stderr)
        return {}

    obs_data = [row["obs"] for row in rows]
    actions = torch.tensor([row["actions"] for row in rows], dtype=torch.long)
    n_total = len(rows)

    # Build wrapped observation space
    wrapped_obs_space = gymnasium.spaces.Dict({
        "observations": game_obs_space,
        "action_mask": gymnasium.spaces.Box(
            low=0.0, high=1.0, shape=(n_actions,), dtype=np.float32,
        ),
    })

    # Build module
    module = HaskboardRLModule(
        observation_space=wrapped_obs_space,
        action_space=act_space,
        model_config=MODEL_CONFIG,
    )
    module.to(device)
    module.train()

    print(f"  {policy_name}: {n_total} samples, input_dim={module._input_dim}", file=sys.stderr)

    # Train/test split
    n_test = max(1, int(math.floor(n_total * args.test_split)))
    n_train = n_total - n_test

    train_obs = obs_data[:n_train]
    test_obs = obs_data[n_train:]
    train_actions = actions[:n_train].to(device)
    test_actions = actions[n_train:].to(device)

    optimizer = torch.optim.Adam(module.parameters(), lr=args.lr)
    batch_size = args.batch_size

    for epoch in range(args.epochs):
        # Training
        module.train()
        train_loss_sum = 0.0
        train_correct = 0
        train_total = 0

        indices = np.random.permutation(n_train)
        for start in range(0, n_train, batch_size):
            batch_idx = indices[start:start + batch_size]
            batch_obs_list = [train_obs[i] for i in batch_idx]
            batch_actions = train_actions[batch_idx]

            collated = _collate_obs(batch_obs_list, game_obs_space, n_actions)
            # Move tensors to device
            collated = _to_device(collated, device)

            batch_input = {Columns.OBS: collated}
            output = module._forward_train(batch_input)
            logits = output[Columns.ACTION_DIST_INPUTS]

            loss = F.cross_entropy(logits, batch_actions)

            optimizer.zero_grad()
            loss.backward()
            optimizer.step()

            n = len(batch_idx)
            train_loss_sum += loss.item() * n
            train_correct += (logits.argmax(dim=-1) == batch_actions).sum().item()
            train_total += n

        # Evaluation
        module.eval()
        test_loss_sum = 0.0
        test_correct = 0
        test_total = 0

        with torch.no_grad():
            for start in range(0, n_test, batch_size):
                end = min(start + batch_size, n_test)
                batch_obs_list = test_obs[start:end]
                batch_actions = test_actions[start:end]

                collated = _collate_obs(batch_obs_list, game_obs_space, n_actions)
                collated = _to_device(collated, device)

                batch_input = {Columns.OBS: collated}
                output = module._forward_train(batch_input)
                logits = output[Columns.ACTION_DIST_INPUTS]

                n_b = end - start
                test_loss_sum += F.cross_entropy(logits, batch_actions).item() * n_b
                test_correct += (logits.argmax(dim=-1) == batch_actions).sum().item()
                test_total += n_b

        train_loss = train_loss_sum / max(train_total, 1)
        train_acc = train_correct / max(train_total, 1)
        test_loss = test_loss_sum / max(test_total, 1)
        test_acc = test_correct / max(test_total, 1)

        print(
            f"    Epoch {epoch + 1:3d}/{args.epochs}  "
            f"train_loss={train_loss:.4f}  train_acc={train_acc:.3f}  "
            f"test_loss={test_loss:.4f}  test_acc={test_acc:.3f}",
            file=sys.stderr,
        )

    # Summary
    random_loss = math.log(n_actions)
    random_acc = 1.0 / n_actions
    print(
        f"    Final: test_loss={test_loss:.4f} (random={random_loss:.4f})  "
        f"test_acc={test_acc:.3f} (random={random_acc:.3f})",
        file=sys.stderr,
    )

    return module.get_state()


def _to_device(obj: Any, device: torch.device) -> Any:
    """Recursively move tensors to device."""
    if isinstance(obj, torch.Tensor):
        return obj.to(device)
    if isinstance(obj, dict):
        return {k: _to_device(v, device) for k, v in obj.items()}
    return obj


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Train BC on haskboard using HaskboardRLModule."
    )
    parser.add_argument("--data", required=True,
                        help="Directory with init_msg.json and bc_data_player_*.jsonl")
    parser.add_argument("--epochs", type=int, default=20)
    parser.add_argument("--batch-size", type=int, default=256)
    parser.add_argument("--lr", type=float, default=1e-3)
    parser.add_argument("--test-split", type=float, default=0.1)
    parser.add_argument("--out", default="python/bc_checkpoint/",
                        help="Output checkpoint directory")
    args = parser.parse_args()

    data_dir = Path(args.data)
    out_dir = Path(args.out)

    # Load init_msg to reconstruct spaces
    with open(data_dir / "init_msg.json", "rb") as f:
        init_msg = orjson.loads(f.read())

    agent_ids: list[int] = init_msg["agents"]
    obs_spaces: dict[int, gymnasium.Space] = {
        int(k): _build_space(v)
        for k, v in init_msg["observationSpaces"].items()
    }
    act_space = _build_space(init_msg["actionSpace"])
    n_actions: int = act_space.n

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    print(f"Device: {device}", file=sys.stderr)
    print(f"Agents: {agent_ids}, actions: {n_actions}", file=sys.stderr)

    for aid in agent_ids:
        policy_name = f"player_{aid}"
        data_path = data_dir / f"bc_data_player_{aid}.jsonl"

        if not data_path.exists():
            print(f"WARNING: {data_path} not found, skipping {policy_name}", file=sys.stderr)
            continue

        print(f"\nTraining {policy_name}...", file=sys.stderr)
        state = train_player(
            policy_name=policy_name,
            data_path=data_path,
            game_obs_space=obs_spaces[aid],
            act_space=act_space,
            n_actions=n_actions,
            args=args,
            device=device,
        )

        if not state:
            continue

        # Save checkpoint
        ckpt_path = out_dir / policy_name
        ckpt_path.mkdir(parents=True, exist_ok=True)
        with open(ckpt_path / "module_state.pkl", "wb") as f:
            pickle.dump(state, f)
        print(f"  Saved: {ckpt_path / 'module_state.pkl'}", file=sys.stderr)

    print(f"\nBC training complete. Checkpoint: {out_dir}", file=sys.stderr)


if __name__ == "__main__":
    sys.stdout.reconfigure(line_buffering=True)
    main()
