"""
AgileRL WebSocket agent client for haskboard.

Connects to Interface.Server (port 9159), identifies as a player,
receives InitMsg then StepMsg/terminal messages, and responds with actions.

Usage:
    uv run python ws_agent.py --checkpoint runs/.../best.pt --player 0
"""

import argparse
import asyncio
import json

import numpy as np
import websockets
from agilerl.algorithms.ippo import IPPO

from haskboard_env import _build_space, _obs_to_numpy
from main import _space_flat_dim, flatten_obs


async def run(checkpoint: str, player_num: int, host: str, port: int, max_seq_len: int) -> None:
    agent_id = f"player_{player_num}"
    agent = IPPO.load(checkpoint, device="cpu")

    async with websockets.connect(f"ws://{host}:{port}") as ws:
        await ws.send(str(player_num))  # identify as PlayerNum

        # Skip the welcome text message
        welcome = await ws.recv()
        print(f"Server: {welcome}")

        init = json.loads(await ws.recv())  # InitMsg
        obs_space = _build_space(init["observationSpace"])
        obs_dim = _space_flat_dim(obs_space, max_seq_len)
        all_agents = [f"player_{i}" for i in init["agents"]]
        print(f"InitMsg received: obs_dim={obs_dim}, agents={all_agents}")

        ep_reward = 0.0
        while True:
            raw = await ws.recv()
            try:
                msg = json.loads(raw)
            except json.JSONDecodeError:
                continue  # skip non-JSON (e.g. "waiting for more players")

            if "msgType" not in msg:
                continue  # GameStateView state update — skip

            if msg["msgType"] == "terminal":
                ep_reward += msg["reward"]
                print(f"Episode done | reward {ep_reward:+.1f}")
                ep_reward = 0.0

            elif msg["msgType"] == "step":
                obs = _obs_to_numpy(msg["observation"], obs_space)
                flat = flatten_obs(obs, obs_space, max_seq_len)
                # IPPO has independent networks per agent; zeros for non-acting agents
                obs_dict = {a: np.zeros(obs_dim, dtype=np.float32) for a in all_agents}
                obs_dict[agent_id] = flat
                actions, _, _, _ = agent.get_action(obs_dict)
                action = int(actions[agent_id][0])
                legal = msg["legalActions"]
                if action not in legal:
                    action = legal[0]  # fallback to first legal action
                await ws.send(json.dumps({"type": "action", "action": action}))
                ep_reward += msg["reward"]  # always 0.0 for non-terminal


def main() -> None:
    p = argparse.ArgumentParser(description="AgileRL agent WebSocket client for haskboard")
    p.add_argument("--checkpoint", required=True, help="Path to .pt checkpoint")
    p.add_argument("--player", type=int, required=True, help="PlayerNum (0-based)")
    p.add_argument("--host", default="127.0.0.1")
    p.add_argument("--port", type=int, default=9159)
    p.add_argument("--max-seq-len", type=int, default=64)
    args = p.parse_args()
    asyncio.run(run(args.checkpoint, args.player, args.host, args.port, args.max_seq_len))


if __name__ == "__main__":
    main()
