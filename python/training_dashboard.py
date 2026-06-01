#!/usr/bin/env python3
"""Static training dashboard — reads metrics.jsonl and produces a 3x3 PNG."""

import json
import sys
from collections import defaultdict
from pathlib import Path

import matplotlib.pyplot as plt


def load_metrics(path: Path) -> dict[str, list[dict]]:
    """Load JSONL and group records by agent_id."""
    by_agent: dict[str, list[dict]] = defaultdict(list)
    with open(path) as f:
        for line in f:
            rec = json.loads(line)
            by_agent[rec["agent_id"]].append(rec)
    # Sort each agent's records by global_step
    for records in by_agent.values():
        records.sort(key=lambda r: r["global_step"])
    return dict(by_agent)


def plot_metric(ax, by_agent, metric, title, ylabel=None, nested_key=None):
    """Plot a single scalar metric per agent on the given axes."""
    for agent_id, records in sorted(by_agent.items()):
        steps = [r["global_step"] for r in records]
        if nested_key:
            vals = [r[metric][nested_key] for r in records]
        else:
            vals = [r[metric] for r in records]
        ax.plot(steps, vals, label=agent_id, alpha=0.8)
    ax.set_title(title)
    ax.set_xlabel("global_step")
    ax.set_ylabel(ylabel or metric)
    ax.legend(fontsize="x-small")
    ax.grid(True, alpha=0.3)


def main():
    metrics_path = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("runs/metrics.jsonl")
    if not metrics_path.exists():
        print(f"Error: {metrics_path} not found", file=sys.stderr)
        sys.exit(1)

    by_agent = load_metrics(metrics_path)
    first_agent = sorted(by_agent.keys())[0]

    fig, axes = plt.subplots(3, 3, figsize=(16, 12))
    fig.suptitle("Training Dashboard", fontsize=14, fontweight="bold")

    # Row 1: policy diagnostics
    plot_metric(axes[0, 0], by_agent, "relative_entropy", "Relative Entropy")
    plot_metric(axes[0, 1], by_agent, "approx_kl", "KL Divergence")
    plot_metric(axes[0, 2], by_agent, "residual_variance", "Residual Variance")

    # Row 2: losses and clipping
    plot_metric(axes[1, 0], by_agent, "value_loss", "Value Loss")
    plot_metric(axes[1, 1], by_agent, "policy_loss", "Policy Loss")
    plot_metric(axes[1, 2], by_agent, "clip_fraction", "Clip Fraction")

    # Row 3: gradients, scores, episode length
    # Gradient norms — single agent, two lines
    ax_grad = axes[2, 0]
    records = by_agent[first_agent]
    steps = [r["global_step"] for r in records]
    ax_grad.plot(steps, [r["actor_grad_norm"] for r in records], label="actor", alpha=0.8)
    ax_grad.plot(steps, [r["critic_grad_norm"] for r in records], label="critic", alpha=0.8)
    ax_grad.set_title(f"Gradient Norms ({first_agent})")
    ax_grad.set_xlabel("global_step")
    ax_grad.set_ylabel("grad norm")
    ax_grad.legend(fontsize="x-small")
    ax_grad.grid(True, alpha=0.3)

    plot_metric(axes[2, 1], by_agent, "episode_score", "Episode Score (mean)", ylabel="score", nested_key="mean")
    plot_metric(axes[2, 2], by_agent, "episode_length", "Episode Length (mean)", ylabel="steps", nested_key="mean")

    fig.tight_layout()
    out_path = metrics_path.parent / "dashboard.png"
    fig.savefig(out_path, dpi=150)
    print(f"Saved {out_path}")
    plt.close(fig)


if __name__ == "__main__":
    main()
