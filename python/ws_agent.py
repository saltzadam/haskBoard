"""
AgileRL WebSocket agent client for haskboard.

Connects to Interface.Server (port 9159), identifies as a player,
receives InitMsg then StepMsg/terminal messages, and responds with actions.

Usage:
    uv run python ws_agent.py --checkpoint runs/_0_20480.pt --player 0
"""

import argparse
import asyncio
import json

import websockets
from agilerl.algorithms.ippo import IPPO

from haskboard_env import _build_space, _obs_to_numpy, _zeros


async def run(checkpoint: str, player_num: int, port: int) -> None:
    agent_id = f"player{player_num}_{player_num}"
    algo = IPPO.load(checkpoint, device="cpu")
    algo.set_training_mode(False)

    uri = f"ws://127.0.0.1:{port}"
    for attempt in range(20):
        try:
            async with websockets.connect(uri) as ws:
                await ws.send(str(player_num))
                welcome = await ws.recv()
                # print(f"Server: {welcome}")

                # InitMsg follows, but non-JSON broadcast frames may arrive first
                while True:
                    try:
                        init = json.loads(await ws.recv())
                        break
                    except json.JSONDecodeError:
                        continue
                obs_space = _build_space(init["observationSpaces"][str(player_num)])
                n_actions = _build_space(init["actionSpace"]).n
                obs = _zeros(obs_space)

                # Warm up the model so the first real inference is fast
                algo.get_action(
                    {agent_id: obs},
                    infos={agent_id: {"action_mask": [1] * n_actions}},
                )

                ep_reward = 0.0
                while True:
                    try:
                        raw = await ws.recv()
                    except websockets.exceptions.ConnectionClosed:
                        # print(f"Episode done | reward {ep_reward:+.1f} (connection closed)")
                        return
                    try:
                        msg = json.loads(raw)
                    except json.JSONDecodeError:
                        continue
                    if "msgType" not in msg:
                        continue  # SendState — skip

                    if msg["msgType"] == "terminal":
                        ep_reward += msg["reward"]
                        # print(f"Episode done | reward {ep_reward:+.1f}")
                        return

                    # StepMsg "step"
                    obs = _obs_to_numpy(msg["observation"], obs_space)
                    legal = msg["legalActions"]
                    mask = [1 if i in legal else 0 for i in range(n_actions)]
                    actions, _, _, _ = algo.get_action(
                        {agent_id: obs},
                        infos={agent_id: {"action_mask": mask}},
                    )
                    action = int(actions[agent_id][0])
                    await ws.send(json.dumps({"type": "action", "action": action}))
                    ep_reward += msg["reward"]
            return
        except (ConnectionRefusedError, OSError):
            await asyncio.sleep(0.2)
    raise RuntimeError(f"Could not connect to server on port {port} after retries")


def main() -> None:
    p = argparse.ArgumentParser(description="AgileRL IPPO WebSocket agent for haskboard")
    p.add_argument("--checkpoint", required=True, help="Path to .pt checkpoint")
    p.add_argument("--player", type=int, required=True, help="PlayerNum (0-based)")
    p.add_argument("--port", type=int, default=9159)
    args = p.parse_args()
    asyncio.run(run(args.checkpoint, args.player, args.port))


if __name__ == "__main__":
    main()
