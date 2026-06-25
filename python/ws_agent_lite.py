"""
Numpy-only WebSocket agent for haskboard inference.

Drop-in replacement for ws_agent_rllib.py — same CLI interface
(--checkpoint, --player, --port). No torch, no ray, no gymnasium needed.

The forward pass discovers MLP layers dynamically from checkpoint state dict
keys, so it adapts to any hidden_dims/trunk_dim configuration automatically.

Dependencies: numpy, websockets (stdlib: json, pickle, asyncio, argparse, pathlib)

Usage:
    python ws_agent_lite.py --checkpoint path/to/checkpoint --player 0
"""

import argparse
import asyncio
import json
import pathlib
import pickle
import sys

import numpy as np
import websockets


# ---------------------------------------------------------------------------
# Checkpoint loading
# ---------------------------------------------------------------------------

def find_policy_state_path(
    checkpoint_dir: pathlib.Path, policy_name: str
) -> pathlib.Path:
    """Locate module_state.pkl for a policy inside a checkpoint.

    Supports two layouts:
      Algo:  checkpoint_dir/learner_group/learner/rl_module/<policy>/module_state.pkl
      BC:    checkpoint_dir/<policy>/module_state.pkl
    """
    algo_path = (
        checkpoint_dir / "learner_group" / "learner" / "rl_module"
        / policy_name / "module_state.pkl"
    )
    if algo_path.exists():
        return algo_path
    bc_path = checkpoint_dir / policy_name / "module_state.pkl"
    if bc_path.exists():
        return bc_path
    available = []
    for search_dir in [
        checkpoint_dir / "learner_group" / "learner" / "rl_module",
        checkpoint_dir,
    ]:
        if search_dir.exists():
            available.extend(p.name for p in search_dir.iterdir() if p.is_dir())
    raise FileNotFoundError(
        f"No module_state.pkl found for {policy_name} in {checkpoint_dir}. "
        f"Available: {available}"
    )


def load_weights(checkpoint_dir: str, policy_name: str) -> dict[str, np.ndarray]:
    """Load checkpoint weights as a dict of float32 numpy arrays."""
    path = find_policy_state_path(pathlib.Path(checkpoint_dir), policy_name)
    with open(path, "rb") as f:
        state = pickle.load(f)
    return {k: np.asarray(v, dtype=np.float32) for k, v in state.items()}


# ---------------------------------------------------------------------------
# Numpy forward pass
# ---------------------------------------------------------------------------

def forward(x: np.ndarray, weights: dict[str, np.ndarray]) -> np.ndarray:
    """MLP forward pass with dynamic layer discovery.

    Discovers trunk layers from state dict keys (trunk.0, trunk.2, trunk.4, ...)
    so it adapts to any hidden_dims / trunk_dim configuration.
    """
    trunk_indices = sorted(
        {int(k.split(".")[1])
         for k in weights
         if k.startswith("trunk.") and k.endswith(".weight")}
    )
    for i in trunk_indices:
        x = x @ weights[f"trunk.{i}.weight"].T + weights[f"trunk.{i}.bias"]
        x = np.maximum(0, x)  # ReLU
    logits = x @ weights["policy_head.weight"].T + weights["policy_head.bias"]
    return logits


# ---------------------------------------------------------------------------
# Observation handling (replaces gymnasium + _build_space + _obs_to_numpy)
# ---------------------------------------------------------------------------

def zeros_for_space(spec: dict):
    """Return a zero-valued array matching a GymSpace JSON spec."""
    t = spec["type"]
    if t == "Discrete":
        return np.int64(0)
    if t == "Box":
        return np.zeros(tuple(spec["shape"]), dtype=np.float32)
    if t == "MultiBinary":
        return np.zeros(spec["n"], dtype=np.int8)
    if t == "MultiDiscrete":
        return np.zeros(len(spec["nvec"]), dtype=np.int64)
    if t == "Dict":
        return {k: zeros_for_space(v) for k, v in spec["spaces"].items()}
    return None


def obs_to_numpy(obs, spec: dict):
    """Convert a JSON observation to numpy arrays using space spec for typing.

    Hidden observations (JSON null) get zeros.
    """
    if obs is None:
        return zeros_for_space(spec)
    t = spec["type"]
    if t == "Discrete":
        return np.int64(obs)
    if t == "Box":
        shape = tuple(spec["shape"])
        result = np.zeros(shape, dtype=np.float32)
        flat = np.array(obs, dtype=np.float32).flatten()
        n = min(flat.size, result.size)
        result.flat[:n] = flat[:n]
        return result
    if t == "MultiBinary":
        return np.array(obs, dtype=np.int8)
    if t == "MultiDiscrete":
        return np.array(obs, dtype=np.int64)
    if t == "Dict":
        return {k: obs_to_numpy(obs.get(k), v) for k, v in spec["spaces"].items()}
    return obs


def encode_obs(obs_dict: dict, obs_spec: dict) -> np.ndarray:
    """Flatten observation dict to a single float vector.

    Replicates HaskboardRLModule._encode: sort keys, skip Discrete(1),
    cast to float32, flatten, concatenate.
    """
    if obs_spec["type"] != "Dict":
        return np.asarray(obs_dict, dtype=np.float32).flatten()

    parts = []
    for key in sorted(obs_spec["spaces"].keys()):
        sub = obs_spec["spaces"][key]
        if sub["type"] == "Discrete" and sub["n"] == 1:
            continue
        val = obs_dict[key]
        parts.append(np.asarray(val, dtype=np.float32).flatten())
    return np.concatenate(parts)


# ---------------------------------------------------------------------------
# WebSocket client loop
# ---------------------------------------------------------------------------

async def run(checkpoint: str, player_num: int, port: int) -> None:
    policy_name = f"player_{player_num}"
    uri = f"ws://127.0.0.1:{port}"

    for attempt in range(20):
        try:
            async with websockets.connect(uri) as ws:
                await ws.send(str(player_num))
                await ws.recv()  # welcome

                # Read InitMsg (skip non-JSON broadcast frames)
                while True:
                    try:
                        init = json.loads(await ws.recv())
                        break
                    except json.JSONDecodeError:
                        continue

                obs_spec = init["observationSpaces"][str(player_num)]
                act_spec = init["actionSpace"]
                n_actions = act_spec["n"]

                print(
                    f"Loading checkpoint for {policy_name}...",
                    file=sys.stderr,
                )
                weights = load_weights(checkpoint, policy_name)
                # Infer input dim from first trunk layer for logging
                first_trunk = sorted(
                    int(k.split(".")[1])
                    for k in weights
                    if k.startswith("trunk.") and k.endswith(".weight")
                )[0]
                input_dim = weights[f"trunk.{first_trunk}.weight"].shape[1]
                print(
                    f"Loaded. Input dim={input_dim}, actions={n_actions}",
                    file=sys.stderr,
                )
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
                        game_obs = obs_to_numpy(msg["observation"], obs_spec)
                        legal = set(msg["legalActions"])
                        mask = np.array(
                            [1.0 if i in legal else 0.0 for i in range(n_actions)],
                            dtype=np.float32,
                        )

                        x = encode_obs(game_obs, obs_spec)
                        logits = forward(x, weights)

                        # Apply action mask (illegal actions -> -inf)
                        inf_mask = np.where(mask > 0, 0.0, -1e10)
                        logits = logits + inf_mask

                        action = int(np.argmax(logits))
                        await ws.send(
                            json.dumps({"type": "action", "action": action})
                        )
            return
        except (ConnectionRefusedError, OSError):
            await asyncio.sleep(0.2)
    raise RuntimeError(
        f"Could not connect to server on port {port} after retries"
    )


def main() -> None:
    p = argparse.ArgumentParser(
        description="Numpy-only WebSocket agent for haskboard"
    )
    p.add_argument(
        "--checkpoint", required=True, help="Path to checkpoint directory"
    )
    p.add_argument(
        "--player", type=int, required=True, help="PlayerNum (0-based)"
    )
    p.add_argument("--port", type=int, default=9159)
    args = p.parse_args()
    asyncio.run(run(args.checkpoint, args.player, args.port))


if __name__ == "__main__":
    main()
