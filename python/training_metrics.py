import math

import torch
from torch import Tensor


def tensor_stats(t: Tensor) -> dict[str, float]:
    """Return {mean, std, min, max} as Python floats."""
    return {
        "mean": t.mean().item(),
        "std": t.std().item() if t.numel() > 1 else 0.0,
        "min": t.min().item(),
        "max": t.max().item(),
    }


def compute_residual_variance(predicted: Tensor, targets: Tensor) -> float:
    """Fraction of target variance unexplained by predictions.

    Returns Var(targets - predicted) / Var(targets).
    A value of 1.0 means the value network explains nothing;
    0.0 means it perfectly predicts returns.
    """
    target_var = targets.var()
    if target_var < 1e-8:
        return 0.0
    return ((targets - predicted).var() / target_var).item()


def compute_relative_entropy(entropy: Tensor, action_space_size: int) -> float:
    """Policy entropy as a fraction of maximum possible entropy.

    Returns entropy / log(action_space_size).
    1.0 = uniform random policy, 0.0 = fully deterministic.
    """
    max_entropy = math.log(action_space_size)
    if max_entropy < 1e-8:
        return 0.0
    return (entropy.mean() / max_entropy).item()


def compute_clip_fraction(ratio: Tensor, clip_coef: float) -> float:
    """Fraction of probability ratios that were clipped by PPO."""
    with torch.no_grad():
        clipped = ((ratio - 1.0).abs() > clip_coef).float().mean()
    return clipped.item()
