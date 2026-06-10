"""Behavioral cloning trainer for haskboard.

Loads BC data from .npz, trains a supervised actor matching the IPPO architecture,
and saves weights for loading into IPPO with --bc-weights.

Usage:
    uv run python train_bc.py --data bc_data.npz --epochs 20 --out bc_actor.pt
"""

import argparse
import math
import sys

import numpy as np
import torch
import yaml

from agilerl.algorithms.core.registry import HyperparameterConfig, RLParameter
from agilerl.utils.algo_utils import (
    preprocess_observation as preprocess_observation_fn,
)

from haskboard_env import make
from ippo_instrumented import InstrumentedIPPO

BINARY_PATH = (
    "/home/adam/haskell/haskboard/dist-newstyle/build/x86_64-linux/ghc-9.10.1"
    "/NoMerci-0.1.0.0/x/NoMerci/build/NoMerci/NoMerci"
)


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Behavioural-cloning trainer for haskboard actors."
    )
    parser.add_argument(
        "--data",
        required=True,
        help="Path to .npz file from collect_bc.py",
    )
    parser.add_argument("--epochs", type=int, default=20)
    parser.add_argument("--batch-size", type=int, default=256)
    parser.add_argument("--lr", type=float, default=1e-3)
    parser.add_argument("--out", default="bc_actor.pt")
    parser.add_argument(
        "--test-split",
        type=float,
        default=0.1,
        help="Fraction of data held out for evaluation (default: 0.1)",
    )
    parser.add_argument(
        "--action-names",
        default=None,
        help="Comma-separated action names for display (e.g. 'Take,Decline')",
    )
    return parser.parse_args()


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------


def main() -> None:
    args = parse_args()

    # ------------------------------------------------------------------
    # 1. Load data
    # ------------------------------------------------------------------
    print(f"Loading data from {args.data} ...")
    data = np.load(args.data)

    obs_keys_npz = sorted(k for k in data.files if k.startswith("obs_"))
    obs_keys_clean = [k[4:] for k in obs_keys_npz]  # strip "obs_" prefix

    actions_np: np.ndarray = data["actions"]
    action_source_np: np.ndarray | None = (
        data["action_source"] if "action_source" in data.files else None
    )
    n_total = len(actions_np)
    print(f"Total samples: {n_total}")
    print(f"Obs keys     : {obs_keys_clean}")
    if action_source_np is not None:
        print(f"Action source : present ({len(action_source_np)} entries)")
    else:
        print(f"Action source : not found (old .npz — per-source breakdown skipped)")

    # ------------------------------------------------------------------
    # 2. Get spaces from a live env
    # ------------------------------------------------------------------
    print(f"\nSpawning env to read spaces ...")
    env = make(BINARY_PATH, shared=False)
    obs_spaces = {a: env.observation_space(a) for a in env.possible_agents}
    act_spaces = {a: env.action_space(a) for a in env.possible_agents}
    agent_ids = env.possible_agents
    env.close()
    print(f"Agents: {agent_ids}")

    # All agents share the same observation/action space structure.
    obs_space = obs_spaces[agent_ids[0]]
    act_space = act_spaces[agent_ids[0]]
    n_actions: int = act_space.n  # type: ignore[attr-defined]
    print(f"Action space size: {n_actions}")

    # ------------------------------------------------------------------
    # 3. Build IPPO to get actor architecture
    # ------------------------------------------------------------------
    with open("ippo.yaml") as f:
        config = yaml.safe_load(f)

    hp_config = HyperparameterConfig(
        lr=RLParameter(min=1e-7, max=0.1),
        batch_size=RLParameter(min=8, max=4096, dtype=int),
    )

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    print(f"Device: {device}")

    ippo = InstrumentedIPPO(
        observation_spaces=obs_spaces,
        action_spaces=act_spaces,
        agent_ids=agent_ids,
        index=0,
        hp_config=hp_config,
        net_config=config["NET_CONFIG"],
        device=device,
    )

    group_id = list(ippo.actors.keys())[0]
    actor = ippo.actors[group_id]
    print(f"Using actor for group: {group_id!r}")

    # ------------------------------------------------------------------
    # 4. Prepare data tensors
    # ------------------------------------------------------------------
    # Build per-key float32 tensors
    obs_tensors: dict[str, torch.Tensor] = {
        k: torch.from_numpy(data[npz_k].astype(np.float32))
        for k, npz_k in zip(obs_keys_clean, obs_keys_npz)
    }
    actions_tensor = torch.from_numpy(actions_np).long()

    # ------------------------------------------------------------------
    # 5. Train / test split (first 90% train, last 10% test)
    # ------------------------------------------------------------------
    n_test = max(1, int(math.floor(n_total * args.test_split)))
    n_train = n_total - n_test

    train_obs = {k: v[:n_train] for k, v in obs_tensors.items()}
    test_obs = {k: v[n_train:] for k, v in obs_tensors.items()}
    train_actions = actions_tensor[:n_train]
    test_actions = actions_tensor[n_train:]
    test_source = action_source_np[n_train:] if action_source_np is not None else None

    print(f"\nTrain samples: {n_train}")
    print(f"Test  samples: {n_test}")

    # ------------------------------------------------------------------
    # 6. Training loop
    # ------------------------------------------------------------------
    num_epochs = args.epochs
    batch_size = args.batch_size

    optimizer = torch.optim.Adam(actor.parameters(), lr=args.lr)

    print(f"\nTraining for {num_epochs} epochs  (lr={args.lr}, batch_size={batch_size})\n")

    for epoch in range(num_epochs):
        # --- Training ---
        actor.train()
        train_loss_sum = 0.0
        train_correct = 0
        train_total = 0

        indices = np.random.permutation(n_train)
        for start in range(0, n_train, batch_size):
            batch_idx = indices[start : start + batch_size]
            batch_obs = {k: train_obs[k][batch_idx].to(device) for k in obs_keys_clean}
            batch_actions = train_actions[batch_idx].to(device)

            # preprocess_observation handles Dict spaces
            processed = preprocess_observation_fn(obs_space, batch_obs, device, False)
            actor(processed)  # forward pass creates actor.dist
            log_probs = actor.action_log_prob(batch_actions)
            loss = -log_probs.mean()  # NLL = cross-entropy for discrete

            optimizer.zero_grad()
            loss.backward()
            optimizer.step()

            n = len(batch_idx)
            train_loss_sum += loss.item() * n
            predicted = actor.head_net.dist.logits.argmax(dim=-1)
            train_correct += (predicted == batch_actions).sum().item()
            train_total += n

        # --- Evaluation ---
        actor.eval()
        test_loss_sum = 0.0
        test_correct = 0
        test_total = 0

        with torch.no_grad():
            for start in range(0, n_test, batch_size):
                batch_idx_t = slice(start, min(start + batch_size, n_test))
                batch_obs_t = {k: test_obs[k][batch_idx_t].to(device) for k in obs_keys_clean}
                batch_actions_t = test_actions[batch_idx_t].to(device)
                processed_t = preprocess_observation_fn(obs_space, batch_obs_t, device, False)
                actor(processed_t)
                log_probs_t = actor.action_log_prob(batch_actions_t)
                n_t = batch_idx_t.stop - batch_idx_t.start
                test_loss_sum += (-log_probs_t.mean()).item() * n_t
                predicted_t = actor.head_net.dist.logits.argmax(dim=-1)
                test_correct += (predicted_t == batch_actions_t).sum().item()
                test_total += n_t

        train_loss = train_loss_sum / train_total
        train_acc = train_correct / train_total
        test_loss = test_loss_sum / test_total
        test_acc = test_correct / test_total

        print(
            f"Epoch {epoch + 1:3d}/{num_epochs}  "
            f"train_loss={train_loss:.4f}  train_acc={train_acc:.3f}  "
            f"test_loss={test_loss:.4f}  test_acc={test_acc:.3f}"
        )

    # ------------------------------------------------------------------
    # 7. Save weights
    # ------------------------------------------------------------------
    torch.save(actor.state_dict(), args.out)
    print(f"\nSaved actor weights to {args.out}")

    # ------------------------------------------------------------------
    # 8. Final summary
    # ------------------------------------------------------------------
    random_baseline_loss = math.log(n_actions)
    random_baseline_acc = 1.0 / n_actions

    print(f"\nFinal test loss    : {test_loss:.4f}")
    print(f"Final test accuracy: {test_acc:.3f}")
    print(f"Random baseline    : loss={random_baseline_loss:.4f}  acc={random_baseline_acc:.3f}")
    print(f"Improvement over random: loss delta={random_baseline_loss - test_loss:+.4f}  "
          f"acc delta={test_acc - random_baseline_acc:+.3f}")

    # ------------------------------------------------------------------
    # 9. Per-class accuracy and confusion matrix
    # ------------------------------------------------------------------
    action_names: list[str] = (
        args.action_names.split(",") if args.action_names else
        [str(i) for i in range(n_actions)]
    )

    # Collect all test predictions in one pass
    all_preds: list[np.ndarray] = []
    all_targets: list[np.ndarray] = []
    actor.eval()
    with torch.no_grad():
        for start in range(0, n_test, batch_size):
            batch_idx_t = slice(start, min(start + batch_size, n_test))
            batch_obs_t = {k: test_obs[k][batch_idx_t].to(device) for k in obs_keys_clean}
            batch_actions_t = test_actions[batch_idx_t].to(device)
            processed_t = preprocess_observation_fn(obs_space, batch_obs_t, device, False)
            actor(processed_t)
            predicted_t = actor.head_net.dist.logits.argmax(dim=-1)
            all_preds.append(predicted_t.cpu().numpy())
            all_targets.append(batch_actions_t.cpu().numpy())

    preds_np = np.concatenate(all_preds)
    targets_np = np.concatenate(all_targets)

    # Per-class accuracy (recall)
    print(f"\nPer-class test accuracy (recall):")
    class_correct: dict[int, int] = {}
    class_total: dict[int, int] = {}
    for c in range(n_actions):
        mask = targets_np == c
        total_c = int(mask.sum())
        correct_c = int((preds_np[mask] == c).sum()) if total_c > 0 else 0
        class_correct[c] = correct_c
        class_total[c] = total_c
        name = action_names[c] if c < len(action_names) else str(c)
        if total_c > 0:
            print(f"  {name:12s} (action {c}): {correct_c:>6d}/{total_c:<6d} = {100.0 * correct_c / total_c:.1f}%")
        else:
            print(f"  {name:12s} (action {c}):      0/0      = n/a")

    # Confusion matrix (rows=true, cols=predicted)
    conf = np.zeros((n_actions, n_actions), dtype=np.int64)
    for t, p in zip(targets_np, preds_np):
        conf[t, p] += 1

    header = "".join(f"  pred_{action_names[c] if c < len(action_names) else str(c):>8s}" for c in range(n_actions))
    print(f"\nConfusion matrix (rows=true, cols=predicted):")
    print(f"{'':>14s}{header}")
    for r in range(n_actions):
        name = action_names[r] if r < len(action_names) else str(r)
        row = "".join(f"  {conf[r, c]:>13d}" for c in range(n_actions))
        print(f"  {name:>12s}{row}")

    # Per-source breakdown
    if test_source is not None:
        source_names = {0: "Hint", 1: "Random", 2: "Agent", 3: "Human"}
        print(f"\nBy action source:")
        for src_id in sorted(set(test_source)):
            mask = test_source == src_id
            total_s = int(mask.sum())
            correct_s = int((preds_np[mask] == targets_np[mask]).sum()) if total_s > 0 else 0
            name = source_names.get(int(src_id), "?")
            if total_s > 0:
                print(f"  {name:8s}: {correct_s:>6d}/{total_s:<6d} = {100.0 * correct_s / total_s:.1f}%")
            else:
                print(f"  {name:8s}:      0/0      = n/a")
    else:
        print(f"\n(No action_source in .npz — per-source breakdown skipped)")


if __name__ == "__main__":
    sys.stdout.reconfigure(line_buffering=True)  # type: ignore[union-attr]
    main()
