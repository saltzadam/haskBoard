"""
PettingZoo AEC environment wrapper for haskboard games.

The Haskell binary is spawned as a subprocess; communication happens over
stdio with newline-delimited JSON.

Protocol (Haskell → Python):
  InitMsg:  {"agents":[0,1,2], "observationSpace":{...}, "actionSpace":{...}}
  StepMsg:  {"msgType":"step",     "agent":0, "observation":{...},
             "legalActions":[0,1], "reward":0.0, "terminated":false, "truncated":false}
  StepMsg:  {"msgType":"terminal", "agent":0, "observation":null,
             "legalActions":[],    "reward":1.0,  "terminated":true,  "truncated":false}

Protocol (Python → Haskell):
  {"type":"action", "action": <int>}
  {"type":"reset"}
"""

from __future__ import annotations

import json
import subprocess
from typing import Any

import gymnasium
import numpy as np
from pettingzoo import AECEnv
from pettingzoo.utils import agent_selector


def _build_space(spec: dict) -> gymnasium.Space:
    """Convert a haskboard GymSpace JSON descriptor to a gymnasium Space."""
    t = spec["type"]
    if t == "Discrete":
        return gymnasium.spaces.Discrete(spec["n"])
    elif t == "Box":
        return gymnasium.spaces.Box(
            low=np.float32(spec["low"]),
            high=np.float32(spec["high"]),
            shape=tuple(spec["shape"]),
            dtype=np.float32,
        )
    elif t == "MultiDiscrete":
        return gymnasium.spaces.MultiDiscrete(np.array(spec["nvec"], dtype=np.int64))
    elif t == "MultiBinary":
        return gymnasium.spaces.MultiBinary(spec["n"])
    elif t == "Sequence":
        return gymnasium.spaces.Sequence(_build_space(spec["space"]))
    elif t == "Dict":
        return gymnasium.spaces.Dict(
            {k: _build_space(v) for k, v in spec["spaces"].items()}
        )
    else:
        raise ValueError(f"Unknown GymSpace type: {t!r}")


def _obs_to_numpy(obs: Any, space: gymnasium.Space) -> Any:
    """Best-effort conversion of a JSON observation value to a numpy array.

    For hidden observations (JSON null) the space's zero tensor is returned.
    """
    if obs is None:
        return _zeros(space)
    if isinstance(space, gymnasium.spaces.Discrete):
        return np.int64(obs)
    elif isinstance(space, gymnasium.spaces.Box):
        return np.array(obs, dtype=space.dtype).reshape(space.shape)
    elif isinstance(space, gymnasium.spaces.MultiDiscrete):
        return np.array(obs, dtype=np.int64)
    elif isinstance(space, gymnasium.spaces.MultiBinary):
        return np.array(obs, dtype=np.int8)
    elif isinstance(space, gymnasium.spaces.Sequence):
        return tuple(_obs_to_numpy(x, space.feature_space) for x in obs)
    elif isinstance(space, gymnasium.spaces.Dict):
        return {k: _obs_to_numpy(obs.get(k), s) for k, s in space.spaces.items()}
    return obs


def _zeros(space: gymnasium.Space) -> Any:
    """Return a zero-valued sample compatible with *space*."""
    if isinstance(space, gymnasium.spaces.Discrete):
        return np.int64(0)
    elif isinstance(space, gymnasium.spaces.Box):
        return np.zeros(space.shape, dtype=space.dtype)
    elif isinstance(space, gymnasium.spaces.MultiDiscrete):
        return np.zeros(space.nvec.shape, dtype=np.int64)
    elif isinstance(space, gymnasium.spaces.MultiBinary):
        return np.zeros(space.n, dtype=np.int8)
    elif isinstance(space, gymnasium.spaces.Sequence):
        return ()
    elif isinstance(space, gymnasium.spaces.Dict):
        return {k: _zeros(s) for k, s in space.spaces.items()}
    return None


class HaskboardEnv(AECEnv):
    """PettingZoo AEC environment backed by a haskboard Haskell process.

    Parameters
    ----------
    binary_path:
        Path to the compiled Haskell executable (must accept ``--stdio`` flag).
    extra_args:
        Additional CLI arguments forwarded to the binary.
    """

    metadata = {"render_modes": [], "name": "haskboard_v0"}

    def __init__(self, binary_path: str, extra_args: list[str] | None = None):
        super().__init__()
        self._binary_path = binary_path
        self._extra_args = extra_args or []
        self._proc: subprocess.Popen | None = None

        # Start the process and read the InitMsg
        self._proc = subprocess.Popen(
            [binary_path, "--stdio"] + self._extra_args,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            bufsize=1,
            text=True,
        )

        init_msg = self._read_msg()
        agent_ids: list[int] = init_msg["agents"]

        self.possible_agents = [f"player_{i}" for i in agent_ids]
        self.agents = list(self.possible_agents)
        self._agent_id_map = {f"player_{i}": i for i in agent_ids}

        obs_space = _build_space(init_msg["observationSpace"])
        act_space = _build_space(init_msg["actionSpace"])
        self.observation_spaces = {a: obs_space for a in self.possible_agents}
        self.action_spaces = {a: act_space for a in self.possible_agents}
        self._action_space_size: int = act_space.n  # type: ignore[attr-defined]

        # Per-agent state
        self._observations: dict[str, Any] = {a: _zeros(obs_space) for a in self.agents}
        self._rewards: dict[str, float] = {a: 0.0 for a in self.agents}
        self._terminations: dict[str, bool] = {a: False for a in self.agents}
        self._truncations: dict[str, bool] = {a: False for a in self.agents}
        self._infos: dict[str, dict] = {a: {} for a in self.agents}
        self._legal_actions: dict[str, list[int]] = {a: [] for a in self.agents}

        self.agent_selection: str = self.agents[0]

    # ------------------------------------------------------------------
    # Low-level I/O
    # ------------------------------------------------------------------

    def _read_msg(self) -> dict:
        assert self._proc and self._proc.stdout
        line = self._proc.stdout.readline()
        if not line:
            raise EOFError("Haskell process closed stdout unexpectedly")
        return json.loads(line)

    def _send(self, msg: dict) -> None:
        assert self._proc and self._proc.stdin
        self._proc.stdin.write(json.dumps(msg) + "\n")
        self._proc.stdin.flush()

    # ------------------------------------------------------------------
    # Protocol helpers
    # ------------------------------------------------------------------

    def _advance(self) -> None:
        """Read the next message from Haskell and update internal state."""
        msg = self._read_msg()
        agent_name = f"player_{msg['agent']}"
        obs_space = self.observation_spaces[agent_name]
        self._observations[agent_name] = _obs_to_numpy(msg["observation"], obs_space)
        self._legal_actions[agent_name] = msg["legalActions"]
        

        if msg["msgType"] == "terminal":
            self._rewards[agent_name] = msg["reward"]
            self._terminations[agent_name] = True
            self._truncations[agent_name] = msg["truncated"]
            # If all agents are terminated, drain remaining terminal messages
            while not all(self._terminations.values()):
                msg2 = self._read_msg()
                a2 = f"player_{msg2['agent']}"
                self._rewards[a2] = msg2["reward"]
                self._terminations[a2] = True
                self._truncations[a2] = msg2["truncated"]
        else:
            self.agent_selection = agent_name

    def _action_mask(self, agent: str) -> np.ndarray:
        mask = np.zeros(self._action_space_size, dtype=np.int8)
        for i in self._legal_actions.get(agent, []):
            mask[i] = 1
        return mask

    # ------------------------------------------------------------------
    # PettingZoo AEC API
    # ------------------------------------------------------------------

    def observation_space(self, agent: str) -> gymnasium.Space:
        return self.observation_spaces[agent]

    def action_space(self, agent: str) -> gymnasium.Space:
        return self.action_spaces[agent]

    def observe(self, agent: str) -> Any:
        return self._observations[agent]

    def reset(
        self,
        seed: int | None = None,
        options: dict | None = None,
    ) -> tuple[dict[str, Any], dict[str, dict]]:
        self._send({"type": "reset"})
        self.agents = list(self.possible_agents)
        obs_space = self.observation_spaces[self.agents[0]]
        self._observations = {a: _zeros(obs_space) for a in self.agents}
        self._rewards = {a: 0.0 for a in self.agents}
        self._terminations = {a: False for a in self.agents}
        self._truncations = {a: False for a in self.agents}
        self._infos = {a: {} for a in self.agents}
        self._legal_actions = {a: [] for a in self.agents}

        self._advance()
        return {a: self._observations[a] for a in self.agents}, dict(self._infos)

    def step(self, action: int) -> None:
        if self._terminations.get(self.agent_selection, False):
            # Dead-step: agent already done
            self._was_dead_step(action)
            return

        self._send({"type": "action", "action": int(action)})
        # Reset reward for current agent before reading next message
        self._rewards[self.agent_selection] = 0.0
        self._advance()

    def last(
        self,
        observe: bool = True,
    ) -> tuple[Any, float, bool, bool, dict]:
        agent = self.agent_selection
        obs = self.observe(agent) if observe else None
        return (
            obs,
            self._rewards[agent],
            self._terminations[agent],
            self._truncations[agent],
            self._infos[agent],
        )

    def close(self) -> None:
        if self._proc is not None:
            self._proc.terminate()
            self._proc = None

    def render(self) -> None:
        pass


# ---------------------------------------------------------------------------
# Convenience factory
# ---------------------------------------------------------------------------

def make(binary_path: str, **kwargs: Any) -> HaskboardEnv:
    """Create a HaskboardEnv.  Pass *binary_path* to the haskboard executable."""
    return HaskboardEnv(binary_path, **kwargs)
