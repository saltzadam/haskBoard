"""
Custom TorchRLModule for haskboard games with action masking.

All observation values (MultiBinary, Box) are cast to float and concatenated
into a single flat vector. A shared MLP trunk feeds separate policy and value
heads. The policy head applies action masking (illegal actions get logits of
-inf).
"""

from __future__ import annotations

from typing import Any

import gymnasium
import numpy as np
import torch
import torch.nn as nn
from ray.rllib.core.columns import Columns
from ray.rllib.core.rl_module.torch.torch_rl_module import TorchRLModule


class HaskboardRLModule(TorchRLModule):
    """Flat-concat RLModule with action masking for haskboard games.

    Model config keys (via ``model_config``):
        hidden_dims: list[int]  -- MLP hidden layer sizes (default [256, 256])
        trunk_dim: int          -- shared trunk output dim (default 128)
    """

    def setup(self) -> None:
        model_config = self.config.model_config_dict or {}
        hidden_dims: list[int] = model_config.get("hidden_dims", [256, 256])
        trunk_dim: int = model_config.get("trunk_dim", 128)

        # Determine input size by inspecting the observation space.
        # The obs space is Dict({"observations": <game_dict>, "action_mask": Box(...)}).
        obs_space = self.config.observation_space
        if isinstance(obs_space, gymnasium.spaces.Dict) and "observations" in obs_space.spaces:
            game_space = obs_space["observations"]
        else:
            game_space = obs_space

        self._sorted_keys: list[str] = []
        input_dim = 0
        if isinstance(game_space, gymnasium.spaces.Dict):
            for key in sorted(game_space.spaces.keys()):
                sub = game_space.spaces[key]
                # Skip Discrete(1) -- dummy/invisible locations
                if isinstance(sub, gymnasium.spaces.Discrete) and sub.n == 1:
                    continue
                self._sorted_keys.append(key)
                if isinstance(sub, gymnasium.spaces.MultiBinary):
                    n = sub.n if isinstance(sub.n, int) else int(np.prod(sub.n))
                    input_dim += n
                elif isinstance(sub, gymnasium.spaces.Box):
                    input_dim += int(np.prod(sub.shape))
                elif isinstance(sub, gymnasium.spaces.Discrete):
                    input_dim += 1
                elif isinstance(sub, gymnasium.spaces.MultiDiscrete):
                    input_dim += len(sub.nvec)
                else:
                    input_dim += int(np.prod(sub.shape))
        else:
            # Fallback: single flat space
            self._sorted_keys = []
            if hasattr(game_space, "n"):
                input_dim = int(game_space.n)
            else:
                input_dim = int(np.prod(game_space.shape))

        self._input_dim = input_dim

        # Action space size
        act_space = self.config.action_space
        if isinstance(act_space, gymnasium.spaces.Discrete):
            n_actions = int(act_space.n)
        else:
            n_actions = int(np.prod(act_space.shape))
        self._n_actions = n_actions

        # Build shared trunk
        layers: list[nn.Module] = []
        prev_dim = input_dim
        for h in hidden_dims:
            layers.append(nn.Linear(prev_dim, h))
            layers.append(nn.ReLU())
            prev_dim = h
        layers.append(nn.Linear(prev_dim, trunk_dim))
        layers.append(nn.ReLU())
        self.trunk = nn.Sequential(*layers)

        # Policy head
        self.policy_head = nn.Linear(trunk_dim, n_actions)

        # Value head
        self.value_head = nn.Linear(trunk_dim, 1)

    def _encode(self, obs_dict: dict[str, torch.Tensor]) -> torch.Tensor:
        """Flatten and concatenate observation dict into a single tensor."""
        parts: list[torch.Tensor] = []
        if self._sorted_keys:
            for key in self._sorted_keys:
                val = obs_dict[key]
                parts.append(val.float().reshape(val.shape[0], -1))
        else:
            # Single-space fallback
            for key in sorted(obs_dict.keys()):
                if key == "action_mask":
                    continue
                val = obs_dict[key]
                parts.append(val.float().reshape(val.shape[0], -1))
        return torch.cat(parts, dim=-1)

    def _forward(self, batch: dict[str, Any], is_inference: bool = False) -> dict[str, Any]:
        obs = batch[Columns.OBS]

        # Extract action mask and game observations
        if isinstance(obs, dict) and "action_mask" in obs:
            action_mask = obs["action_mask"]
            game_obs = obs.get("observations", obs)
        else:
            action_mask = None
            game_obs = obs

        # Encode observations
        if isinstance(game_obs, dict):
            x = self._encode(game_obs)
        else:
            x = game_obs.float().reshape(game_obs.shape[0], -1)

        # Shared trunk
        trunk_out = self.trunk(x)

        # Policy logits with action masking
        logits = self.policy_head(trunk_out)
        if action_mask is not None:
            inf_mask = torch.clamp(torch.log(action_mask.float()), min=-1e10)
            logits = logits + inf_mask

        output = {Columns.ACTION_DIST_INPUTS: logits}

        # Value predictions (always compute for training, skip for pure inference)
        if not is_inference:
            output[Columns.VF_PREDS] = self.value_head(trunk_out).squeeze(-1)

        return output

    def _forward_inference(self, batch: dict[str, Any], **kwargs: Any) -> dict[str, Any]:
        return self._forward(batch, is_inference=True)

    def _forward_exploration(self, batch: dict[str, Any], **kwargs: Any) -> dict[str, Any]:
        return self._forward(batch, is_inference=False)

    def _forward_train(self, batch: dict[str, Any], **kwargs: Any) -> dict[str, Any]:
        return self._forward(batch, is_inference=False)

    def compute_values(self, batch: dict[str, Any]) -> torch.Tensor:
        obs = batch[Columns.OBS]
        if isinstance(obs, dict) and "observations" in obs:
            game_obs = obs["observations"]
        else:
            game_obs = obs

        if isinstance(game_obs, dict):
            x = self._encode(game_obs)
        else:
            x = game_obs.float().reshape(game_obs.shape[0], -1)

        trunk_out = self.trunk(x)
        return self.value_head(trunk_out).squeeze(-1)
