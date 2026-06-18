"""
RLlib WebSocket agent client for haskboard.

Connects to Interface.Server (port 9159), identifies as a player,
receives InitMsg then StepMsg/terminal messages, and responds with actions
using a trained HaskboardRLModule loaded from an RLlib checkpoint.

No Ray cluster needed -- loads checkpoint weights directly via PyTorch.

Usage:
    uv run --project python python python/ws_agent_rllib.py \
        --checkpoint python/ray_results/PPO_.../checkpoint_000010 \
        --player 0
"""

import argparse
import asyncio
import json
import pathlib
import pickle
import sys

import gymnasium
import numpy as np
import torch
import websockets

from ray.rllib.core.columns import Columns

from haskboard_aec_env import _build_space, _obs_to_numpy
from haskboard_rl_module import HaskboardRLModule

# Training model config -- must match train_rllib.py
MODEL_CONFIG = {"hidden_dims": [256, 256], "trunk_dim": 128}


def find_policy_state_path(checkpoint_dir: pathlib.Path, policy_name: str) -> pathlib.Path:
    """Locate the module_state.pkl for a policy inside a checkpoint.

    Supports two layouts:
      Algo checkpoint:  checkpoint_dir/learner_group/learner/rl_module/<policy>/module_state.pkl
      BC checkpoint:    checkpoint_dir/<policy>/module_state.pkl
    """
    # Try algo checkpoint layout first
    algo_path = checkpoint_dir / "learner_group" / "learner" / "rl_module" / policy_name / "module_state.pkl"
    if algo_path.exists():
        return algo_path
    # Try flat BC checkpoint layout
    bc_path = checkpoint_dir / policy_name / "module_state.pkl"
    if bc_path.exists():
        return bc_path
    # List what's available for a helpful error
    available = []
    for search_dir in [checkpoint_dir / "learner_group" / "learner" / "rl_module", checkpoint_dir]:
        if search_dir.exists():
            available.extend(p.name for p in search_dir.iterdir() if p.is_dir())
    raise FileNotFoundError(
        f"No module_state.pkl found for {policy_name} in {checkpoint_dir}. "
        f"Available: {available}"
    )


def load_module(
    checkpoint_dir: str,
    policy_name: str,
    obs_space: gymnasium.spaces.Dict,
    act_space: gymnasium.Space,
) -> HaskboardRLModule:
    """Build a HaskboardRLModule and load trained weights from checkpoint."""
    ckpt = pathlib.Path(checkpoint_dir)
    state_file = find_policy_state_path(ckpt, policy_name)

    module = HaskboardRLModule(
        observation_space=obs_space,
        action_space=act_space,
        model_config=MODEL_CONFIG,
    )

    with open(state_file, "rb") as f:
        state = pickle.load(f)
    module.set_state(state)
    module.eval()
    return module


async def run(checkpoint: str, player_num: int, port: int) -> None:
    policy_name = f"player_{player_num}"
    uri = f"ws://127.0.0.1:{port}"

    for attempt in range(20):
        try:
            async with websockets.connect(uri) as ws:
                await ws.send(str(player_num))
                await ws.recv()  # welcome

                # InitMsg follows; skip non-JSON broadcast frames
                while True:
                    try:
                        init = json.loads(await ws.recv())
                        break
                    except json.JSONDecodeError:
                        continue

                raw_obs_space = _build_space(init["observationSpaces"][str(player_num)])
                act_space = _build_space(init["actionSpace"])
                n_actions = act_space.n

                # Wrap in the Dict(observations=..., action_mask=...) expected by the module
                wrapped_obs_space = gymnasium.spaces.Dict({
                    "observations": raw_obs_space,
                    "action_mask": gymnasium.spaces.Box(
                        low=0.0, high=1.0,
                        shape=(n_actions,),
                        dtype=np.float32,
                    ),
                })

                print(f"Loading RLlib checkpoint for {policy_name}...", file=sys.stderr)
                module = load_module(checkpoint, policy_name, wrapped_obs_space, act_space)
                print(f"Loaded. Input dim={module._input_dim}, actions={n_actions}", file=sys.stderr)
                await ws.send(json.dumps({"type": "ready"}))

                while True:  # game loop
                    while True:  # message loop
                        try:
                            raw = await ws.recv()
                        except websockets.exceptions.ConnectionClosed:
                            return
                        try:
                            msg = json.loads(raw)
                        except json.JSONDecodeError:
                            continue
                        if "msgType" not in msg:
                            continue  # SendState -- skip

                        if msg["msgType"] == "terminal":
                            break

                        # StepMsg -- run inference
                        game_obs = _obs_to_numpy(msg["observation"], raw_obs_space)
                        legal = msg["legalActions"]
                        mask = np.array(
                            [1.0 if i in legal else 0.0 for i in range(n_actions)],
                            dtype=np.float32,
                        )

                        # Build batch: {OBS: {"observations": {...}, "action_mask": ...}}
                        if isinstance(game_obs, dict):
                            obs_tensors = {
                                k: torch.tensor(np.asarray(v), dtype=torch.float32).unsqueeze(0)
                                for k, v in game_obs.items()
                            }
                        else:
                            obs_tensors = torch.tensor(
                                np.asarray(game_obs), dtype=torch.float32
                            ).unsqueeze(0)

                        batch = {
                            Columns.OBS: {
                                "observations": obs_tensors,
                                "action_mask": torch.tensor(mask).unsqueeze(0),
                            }
                        }

                        with torch.no_grad():
                            output = module._forward_inference(batch)

                        logits = output[Columns.ACTION_DIST_INPUTS][0]
                        action = int(torch.argmax(logits))
                        await ws.send(json.dumps({"type": "action", "action": action}))
            return
        except (ConnectionRefusedError, OSError):
            await asyncio.sleep(0.2)
    raise RuntimeError(f"Could not connect to server on port {port} after retries")


def main() -> None:
    p = argparse.ArgumentParser(description="RLlib WebSocket agent for haskboard")
    p.add_argument("--checkpoint", required=True,
                    help="Path to RLlib algo checkpoint root (dir containing learner_group/)")
    p.add_argument("--player", type=int, required=True,
                    help="PlayerNum (0-based)")
    p.add_argument("--port", type=int, default=9159)
    args = p.parse_args()
    asyncio.run(run(args.checkpoint, args.player, args.port))


if __name__ == "__main__":
    main()
