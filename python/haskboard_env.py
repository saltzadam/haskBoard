"""
PettingZoo AEC environment wrapper for haskboard games.

The Haskell binary is spawned as a subprocess; communication happens over
stdio with newline-delimited JSON.

Protocol (Haskell → Python):
  InitMsg:  {"agents":[0,1,2], "observationSpaces":{"0":{...},"1":{...}}, "actionSpace":{...}}
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
from pettingzoo import ParallelEnv


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
    elif t == "Sequence":
        inner = _build_space(spec["space"])
        if isinstance(inner, gymnasium.spaces.Discrete):
            # agilerl doesn't support Sequence spaces; convert to MultiBinary histogram
            return gymnasium.spaces.MultiBinary(int(inner.n))
        return gymnasium.spaces.Sequence(inner)
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
        return np.array([obs], dtype=np.int64)
    elif isinstance(space, gymnasium.spaces.Box):
        result = np.zeros(space.shape, dtype=space.dtype)
        flat = np.array(obs, dtype=space.dtype).flatten()
        n = min(flat.size, result.size)
        result.flat[:n] = flat[:n]
        return result
    elif isinstance(space, gymnasium.spaces.MultiDiscrete):
        return np.array(obs, dtype=np.int64)
    elif isinstance(space, gymnasium.spaces.MultiBinary):
        # MultiBinary is only produced by converting a Sequence(Discrete(n));
        # obs is a list of resource indices present in the deck.
        arr = np.zeros(space.n, dtype=np.int8)
        for idx in obs:
            if 0 <= int(idx) < space.n:
                arr[int(idx)] = 1
        return arr
    elif isinstance(space, gymnasium.spaces.Sequence):
        return tuple(_obs_to_numpy(x, space.feature_space) for x in obs)
    elif isinstance(space, gymnasium.spaces.Dict):
        return {k: _obs_to_numpy(obs.get(k), s) for k, s in space.spaces.items()}
    return obs


# ---------------------------------------------------------------------------
# Dict-preserving float32 conversion ("boxify")
#
# Every leaf space becomes a float32 Box so NaN works in shared memory,
# while the Dict wrapper is preserved for EvolvableMultiInput.
# ---------------------------------------------------------------------------


def _pad_shape(shape: tuple[int, ...]) -> tuple[int, ...]:
    """Ensure shape has no dimension of size 1.

    AgileRL's reshape_from_space squeezes any trailing dim==1, which corrupts
    the batch dimension for shape-(1,) observations.  Padding to (2,) avoids
    this with minimal overhead.
    """
    if shape == (1,):
        return (2,)
    return shape


def _boxify_space(space: gymnasium.Space) -> gymnasium.Space:
    """Convert a space to all-float32-Box leaves, preserving Dict structure."""
    if isinstance(space, gymnasium.spaces.Dict):
        return gymnasium.spaces.Dict(
            {k: _boxify_space(s) for k, s in space.spaces.items()}
        )
    if isinstance(space, gymnasium.spaces.Box):
        shape = _pad_shape(space.shape)
        low = np.zeros(shape, dtype=np.float32)
        high = np.ones(shape, dtype=np.float32)
        low[:space.shape[0]] = space.low.astype(np.float32).flat[:space.shape[0]]
        high[:space.shape[0]] = space.high.astype(np.float32).flat[:space.shape[0]]
        return gymnasium.spaces.Box(low=low, high=high, dtype=np.float32)
    if isinstance(space, gymnasium.spaces.Discrete):
        shape = _pad_shape((1,))
        low = np.zeros(shape, dtype=np.float32)
        high = np.zeros(shape, dtype=np.float32)
        high[0] = np.float32(space.n - 1)
        return gymnasium.spaces.Box(low=low, high=high, dtype=np.float32)
    if isinstance(space, gymnasium.spaces.MultiDiscrete):
        highs = (space.nvec - 1).astype(np.float32)
        shape = _pad_shape(highs.shape)
        low = np.zeros(shape, dtype=np.float32)
        high = np.zeros(shape, dtype=np.float32)
        high[:len(highs)] = highs
        return gymnasium.spaces.Box(low=low, high=high, dtype=np.float32)
    if isinstance(space, gymnasium.spaces.MultiBinary):
        n = space.n if isinstance(space.n, int) else int(np.prod(space.n))
        shape = _pad_shape((n,))
        return gymnasium.spaces.Box(
            low=np.float32(0), high=np.float32(1),
            shape=shape, dtype=np.float32,
        )
    # Fallback
    shape = _pad_shape((1,))
    return gymnasium.spaces.Box(low=np.zeros(shape, dtype=np.float32),
                                high=np.ones(shape, dtype=np.float32),
                                dtype=np.float32)


def _boxify_obs(obs: Any, raw_space: gymnasium.Space) -> Any:
    """Convert a parsed observation to float32 arrays matching _boxify_space."""
    if isinstance(raw_space, gymnasium.spaces.Dict):
        return {
            k: _boxify_obs(obs[k] if isinstance(obs, dict) else obs, s)
            for k, s in raw_space.spaces.items()
        }
    # Leaf: convert to float32, pad with zero if needed
    target_shape = _boxify_space(raw_space).shape
    arr = np.asarray(obs, dtype=np.float32).flatten()
    result = np.zeros(target_shape, dtype=np.float32)
    n = min(arr.size, result.size)
    result.flat[:n] = arr[:n]
    return result


def _zeros_for(space: gymnasium.Space) -> Any:
    """Return zero observation matching a boxified space."""
    if isinstance(space, gymnasium.spaces.Dict):
        return {k: _zeros_for(s) for k, s in space.spaces.items()}
    return np.zeros(space.shape, dtype=np.float32)


def _nans_for(space: gymnasium.Space) -> Any:
    """Return NaN observation matching a boxified space."""
    if isinstance(space, gymnasium.spaces.Dict):
        return {k: _nans_for(s) for k, s in space.spaces.items()}
    return np.full(space.shape, np.nan, dtype=np.float32)


def _zeros(space: gymnasium.Space) -> Any:
    """Return a zero-valued sample compatible with *space*."""
    if isinstance(space, gymnasium.spaces.Discrete):
        return np.array([0], dtype=np.int64)
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


class HaskboardEnv(ParallelEnv):
    """PettingZoo Parallel environment backed by a haskboard Haskell process.

    Parameters
    ----------
    binary_path:
        Path to the compiled Haskell executable (must accept ``--stdio`` flag).
    extra_args:
        Additional CLI arguments forwarded to the binary.
    """

    metadata = {"render_modes": [], "name": "haskboard_v0"}
    render_mode = None

    def __init__(self, binary_path: str, extra_args: list[str] | None = None, shared: bool = True):
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

        # Shared: "player_0", "player_1" → group "player" → one shared network.
        # Independent: "player0_0", "player1_1" → groups "player0","player1" → separate networks.
        self._player_fmt = lambda i: f"player_{i}" if shared else f"player{i}_{i}"

        self.possible_agents = [self._player_fmt(i) for i in agent_ids]
        self.agents = list(self.possible_agents)
        self._agent_id_map = {self._player_fmt(i): i for i in agent_ids}

        obs_spaces_raw = init_msg["observationSpaces"]  # {"0": {...}, "1": {...}, ...}
        act_space = _build_space(init_msg["actionSpace"])
        # Keep original (structured) spaces for parsing JSON observations
        self._raw_obs_spaces = {
            self._player_fmt(int(i)): _build_space(s)
            for i, s in obs_spaces_raw.items()
        }
        # Expose Dict of float32 Box subspaces (NaN-safe for shared memory,
        # preserves structure for EvolvableMultiInput)
        self.observation_spaces = {
            a: _boxify_space(s) for a, s in self._raw_obs_spaces.items()
        }
        self.action_spaces = {a: act_space for a in self.possible_agents}
        self._action_space_size: int = act_space.n  # type: ignore[attr-defined]

        # Per-agent state
        self._observations: dict[str, Any] = {a: _zeros_for(self.observation_spaces[a]) for a in self.agents}
        self._rewards: dict[str, float] = {a: 0.0 for a in self.agents}
        # True = no game in progress (so first reset() skips the drain check)
        self._terminations: dict[str, bool] = {a: True for a in self.agents}
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
        agent_name = self._player_fmt(msg['agent'])
        raw_space = self._raw_obs_spaces[agent_name]
        raw_obs = _obs_to_numpy(msg["observation"], raw_space)
        self._observations[agent_name] = _boxify_obs(raw_obs, raw_space)
        self._legal_actions[agent_name] = msg["legalActions"]

        if msg["msgType"] == "terminal":
            self._rewards[agent_name] = msg["reward"]
            self._terminations[agent_name] = True
            self._truncations[agent_name] = msg["truncated"]
            # If all agents are terminated, drain remaining terminal messages
            while not all(self._terminations.values()):
                msg2 = self._read_msg()
                a2 = self._player_fmt(msg2['agent'])
                self._rewards[a2] = msg2["reward"]
                self._terminations[a2] = True
                self._truncations[a2] = msg2["truncated"]
        else:
            self.agent_selection = agent_name

    def _drain_to_terminal(self) -> None:
        """Send legal actions until Haskell reaches a terminal state.

        Called when max_steps truncates an episode mid-game: Haskell is still
        blocked in readAction, so we must satisfy it before sending a reset.
        """
        while not all(self._terminations.values()):
            legal = self._legal_actions.get(self.agent_selection, [])
            action = legal[0] if legal else 0
            self._send({"type": "action", "action": action})
            self._advance()

    def _action_mask(self, agent: str) -> np.ndarray:
        mask = np.zeros(self._action_space_size, dtype=np.int8)
        for i in self._legal_actions.get(agent, []):
            mask[i] = 1
        return mask

    # ------------------------------------------------------------------
    # PettingZoo Parallel API
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
        if not all(self._terminations.values()):
            self._drain_to_terminal()
        self._send({"type": "reset"})
        self.agents = list(self.possible_agents)
        self._observations = {a: _zeros_for(self.observation_spaces[a]) for a in self.agents}
        self._rewards = {a: 0.0 for a in self.agents}
        self._terminations = {a: False for a in self.agents}
        self._truncations = {a: False for a in self.agents}
        self._infos = {a: {} for a in self.agents}
        self._legal_actions = {a: [] for a in self.agents}

        self._advance()
        # Mark inactive agents with nan (only the first-to-act has real obs)
        for a in self.agents:
            if a != self.agent_selection:
                self._observations[a] = _nans_for(self.observation_spaces[a])
                self._rewards[a] = np.nan
        return {a: self._observations[a] for a in self.agents}, dict(self._infos)

    def step(self, actions: dict[str, Any]) -> tuple[dict, dict, dict, dict, dict]:
        if all(self._terminations.values()):
            obs = {a: self._observations[a] for a in self.possible_agents}
            return obs, dict(self._rewards), dict(self._terminations), dict(self._truncations), dict(self._infos)

        prev_active = self.agent_selection
        action = int(actions[prev_active])
        self._send({"type": "action", "action": action})
        self._rewards[prev_active] = 0.0
        self._advance()

        # Mark inactive agents with nan so AsyncAgentsWrapper can filter them.
        # Only the newly active agent gets real obs; prev_active keeps its reward.
        next_active = self.agent_selection if not all(self._terminations.values()) else None
        for a in self.possible_agents:
            if a == next_active or self._terminations[a]:
                continue
            # NaN observation for all non-active agents (including prev_active)
            self._observations[a] = _nans_for(self.observation_spaces[a])
            # NaN reward only for agents that didn't just act
            if a != prev_active:
                self._rewards[a] = np.nan

        obs = {a: self._observations[a] for a in self.possible_agents}
        return obs, dict(self._rewards), dict(self._terminations), dict(self._truncations), dict(self._infos)

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
