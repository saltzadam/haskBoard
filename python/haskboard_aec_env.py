"""
PettingZoo AEC environment wrapper for haskboard games.

The Haskell binary is spawned as a subprocess; communication happens over
stdio with newline-delimited JSON.

Protocol (Haskell -> Python):
  InitMsg:  {"agents":[0,1,2], "observationSpaces":{"0":{...},"1":{...}}, "actionSpace":{...}}
  StepMsg:  {"msgType":"step",     "agent":0, "observation":{...},
             "legalActions":[0,1], "reward":0.0, "terminated":false, "truncated":false}
  StepMsg:  {"msgType":"terminal", "agent":0, "observation":null,
             "legalActions":[],    "reward":1.0,  "terminated":true,  "truncated":false}

Protocol (Python -> Haskell):
  {"type":"action", "action": <int>}
  {"type":"reset"}
"""

from __future__ import annotations

import subprocess
from typing import Any

import gymnasium
import numpy as np
import orjson
from pettingzoo import AECEnv


def _build_space(spec: dict) -> gymnasium.Space:
    """Convert a haskboard GymSpace JSON descriptor to a gymnasium Space."""
    t = spec["type"]
    if t == "Discrete":
        return gymnasium.spaces.Discrete(spec["n"])
    elif t == "Box":
        shape = tuple(spec["shape"])
        return gymnasium.spaces.Box(
            low=np.float32(spec["low"]),
            high=np.float32(spec["high"]),
            shape=shape,
            dtype=np.float32,
        )
    elif t == "MultiDiscrete":
        return gymnasium.spaces.MultiDiscrete(np.array(spec["nvec"], dtype=np.int64))
    elif t == "MultiBinary":
        return gymnasium.spaces.MultiBinary(spec["n"])
    elif t == "Dict":
        return gymnasium.spaces.Dict(
            {k: _build_space(v) for k, v in spec["spaces"].items()}
        )
    else:
        raise ValueError(f"Unknown GymSpace type: {t!r}")


def _obs_to_numpy(obs: Any, space: gymnasium.Space) -> Any:
    """Convert a JSON observation value to a numpy array.

    For hidden observations (JSON null) the space's zero tensor is returned.
    """
    if obs is None:
        return _zeros(space)
    if isinstance(space, gymnasium.spaces.Discrete):
        return np.int64(obs)
    elif isinstance(space, gymnasium.spaces.Box):
        result = np.zeros(space.shape, dtype=space.dtype)
        flat = np.array(obs, dtype=space.dtype).flatten()
        n = min(flat.size, result.size)
        result.flat[:n] = flat[:n]
        return result
    elif isinstance(space, gymnasium.spaces.MultiDiscrete):
        return np.array(obs, dtype=np.int64)
    elif isinstance(space, gymnasium.spaces.MultiBinary):
        return np.array(obs, dtype=np.int8)
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
    elif isinstance(space, gymnasium.spaces.Dict):
        return {k: _zeros(s) for k, s in space.spaces.items()}
    return None


class HaskboardAECEnv(AECEnv):
    """PettingZoo AEC environment backed by a haskboard Haskell process.

    Parameters
    ----------
    binary_path:
        Path to the compiled Haskell executable (must accept ``--stdio`` flag).
    extra_args:
        Additional CLI arguments forwarded to the binary.
    """

    metadata = {"render_modes": [], "name": "haskboard_aec_v0"}
    render_mode = None

    def __init__(
        self,
        binary_path: str,
        extra_args: list[str] | None = None,
        num_players: int | None = None,
    ):
        super().__init__()
        self._binary_path = binary_path
        self._extra_args = extra_args or []
        if num_players is not None:
            self._extra_args += ["--players", str(num_players)]
        self._proc: subprocess.Popen | None = None

        # Start the process and read the InitMsg
        self._proc = subprocess.Popen(
            [binary_path, "--stdio"] + self._extra_args,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            bufsize=0,
        )

        init_msg = self._read_msg()
        agent_ids: list[int] = init_msg["agents"]

        self.possible_agents = [f"player_{i}" for i in agent_ids]
        self.agents: list[str] = list(self.possible_agents)
        self._agent_id_map = {f"player_{i}": i for i in agent_ids}

        obs_spaces_raw = init_msg["observationSpaces"]
        act_space_spec = init_msg["actionSpace"]
        self._act_space = _build_space(act_space_spec)
        self._action_space_size: int = self._act_space.n  # type: ignore[attr-defined]

        # Build per-agent observation spaces (the game Dict)
        self._game_obs_spaces: dict[str, gymnasium.Space] = {
            f"player_{int(i)}": _build_space(s)
            for i, s in obs_spaces_raw.items()
        }

        # Wrapped obs space: Dict({"observations": <game_dict>, "action_mask": Box(...)})
        self._obs_spaces: dict[str, gymnasium.spaces.Dict] = {}
        for agent in self.possible_agents:
            self._obs_spaces[agent] = gymnasium.spaces.Dict({
                "observations": self._game_obs_spaces[agent],
                "action_mask": gymnasium.spaces.Box(
                    low=0.0, high=1.0,
                    shape=(self._action_space_size,),
                    dtype=np.float32,
                ),
            })

        # Per-agent state
        self._observations: dict[str, Any] = {}
        self.rewards: dict[str, float] = {}
        self.terminations: dict[str, bool] = {}
        self.truncations: dict[str, bool] = {}
        self.infos: dict[str, dict] = {}
        self._legal_actions: dict[str, list[int]] = {}
        self._cumulative_rewards: dict[str, float] = {}

        self.agent_selection: str = self.possible_agents[0]

        # Track whether we need to drain before reset
        self._game_over = True

    # ------------------------------------------------------------------
    # Low-level I/O
    # ------------------------------------------------------------------

    def _read_msg(self) -> dict:
        assert self._proc and self._proc.stdout
        line = self._proc.stdout.readline()
        if not line:
            raise EOFError("Haskell process closed stdout unexpectedly")
        return orjson.loads(line)

    def _send(self, msg: dict) -> None:
        assert self._proc and self._proc.stdin
        self._proc.stdin.write(orjson.dumps(msg) + b"\n")
        self._proc.stdin.flush()

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    def _action_mask(self, agent: str) -> np.ndarray:
        mask = np.zeros(self._action_space_size, dtype=np.float32)
        for i in self._legal_actions.get(agent, []):
            mask[i] = 1.0
        return mask

    def _advance(self) -> None:
        """Read the next message from Haskell and update internal state."""
        msg = self._read_msg()
        agent_name = f"player_{msg['agent']}"
        game_space = self._game_obs_spaces[agent_name]
        game_obs = _obs_to_numpy(msg["observation"], game_space)
        self._observations[agent_name] = game_obs
        self._legal_actions[agent_name] = msg["legalActions"]

        if msg["msgType"] == "terminal":
            self.rewards[agent_name] = msg["reward"]
            self.terminations[agent_name] = True
            self.truncations[agent_name] = msg["truncated"]
            # Drain remaining terminal messages for all agents
            while not all(self.terminations.get(a, False) for a in self.possible_agents):
                msg2 = self._read_msg()
                a2 = f"player_{msg2['agent']}"
                raw_obs2 = _obs_to_numpy(msg2["observation"], self._game_obs_spaces[a2])
                self._observations[a2] = raw_obs2
                self.rewards[a2] = msg2["reward"]
                self.terminations[a2] = True
                self.truncations[a2] = msg2["truncated"]
                self._legal_actions[a2] = msg2.get("legalActions", [])
            self._game_over = True
            # Do NOT clear self.agents here -- let PettingZoo's _was_dead_step
            # remove agents one by one as the wrapper calls step(None).
            # Set agent_selection to the first terminated agent so the wrapper
            # can iterate through them.
            self.agent_selection = agent_name
        else:
            self.agent_selection = agent_name

    def _drain_to_terminal(self) -> None:
        """Send legal actions until Haskell reaches a terminal state.

        Called when we need to reset but the game has not ended yet.
        """
        while not self._game_over:
            legal = self._legal_actions.get(self.agent_selection, [])
            action = legal[0] if legal else 0
            self._send({"type": "action", "action": action})
            self._advance()

    # ------------------------------------------------------------------
    # PettingZoo AEC API
    # ------------------------------------------------------------------

    def observation_space(self, agent: str) -> gymnasium.spaces.Dict:
        return self._obs_spaces[agent]

    def action_space(self, agent: str) -> gymnasium.Space:
        return self._act_space

    def observe(self, agent: str) -> dict[str, Any]:
        game_obs = self._observations.get(agent, _zeros(self._game_obs_spaces[agent]))
        return {
            "observations": game_obs,
            "action_mask": self._action_mask(agent),
        }

    def last(self, observe: bool = True) -> tuple[Any, float, bool, bool, dict[str, Any]]:
        agent = self.agent_selection
        observation = self.observe(agent) if observe else None
        reward = self._cumulative_rewards.get(agent, 0.0)
        terminated = self.terminations.get(agent, False)
        truncated = self.truncations.get(agent, False)
        info = self.infos.get(agent, {})
        return observation, reward, terminated, truncated, info

    def reset(
        self,
        seed: int | None = None,
        options: dict | None = None,
    ) -> None:
        if not self._game_over:
            self._drain_to_terminal()

        self._send({"type": "reset"})
        self.agents = list(self.possible_agents)
        self._game_over = False

        self._observations = {
            a: _zeros(self._game_obs_spaces[a]) for a in self.possible_agents
        }
        self.rewards = {a: 0.0 for a in self.possible_agents}
        self._cumulative_rewards = {a: 0.0 for a in self.possible_agents}
        self.terminations = {a: False for a in self.possible_agents}
        self.truncations = {a: False for a in self.possible_agents}
        self.infos = {a: {} for a in self.possible_agents}
        self._legal_actions = {a: [] for a in self.possible_agents}

        self._advance()

    def step(self, action: int) -> None:
        if self.terminations.get(self.agent_selection, False) or \
           self.truncations.get(self.agent_selection, False):
            # Agent already done; this is a no-op required by PettingZoo
            self._was_dead_step(action)
            return

        agent = self.agent_selection
        self._send({"type": "action", "action": int(action)})

        # Clear reward for acting agent before advancing
        self.rewards[agent] = 0.0

        self._advance()

        # Accumulate rewards
        self._cumulative_rewards = {
            a: self._cumulative_rewards.get(a, 0.0) + self.rewards.get(a, 0.0)
            for a in self.possible_agents
        }

    def close(self) -> None:
        if self._proc is not None:
            self._proc.terminate()
            self._proc = None

    def render(self) -> None:
        pass
