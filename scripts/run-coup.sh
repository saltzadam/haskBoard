#!/bin/bash
# Play Coup locally.
#
#   ./run-coup.sh [N]             play vs RANDOM agents in the TUI (default 3 players)
#   ./run-coup.sh --trained [N]   play vs TRAINED agents (needs python/runs/coup_v1_<N>/)
#   ./run-coup.sh --auto [N]      headless random self-play, prints the winner
#
# --trained spawns the RLlib agent via `uv run --project python`, so uv + the
# trained checkpoint must exist. You are always seat 0 (Player One).
set -e
cd "$(dirname "$0")"
# cabal (ghcup) + uv (the game shells out to `uv` to launch the trained agent).
export PATH="$HOME/.ghcup/bin:/opt/homebrew/bin:$PATH"
mkdir -p logs

case "$1" in
  --trained)
    N="${2:-3}"
    CKPT="python/runs/coup_v1_${N}/rllib_checkpoints"
    if [ ! -d "$CKPT" ]; then
      echo "No trained checkpoint for $N players at $CKPT" >&2
      echo "Train one first:" >&2
      echo "  cd python && uv run python train_rllib.py --name coup_v1_${N} --num-players ${N} \\" >&2
      echo "      --num-env-runners 4 --train-steps 1000 --binary \"\$(cd .. && cabal list-bin Coup)\"" >&2
      exit 1
    fi
    exec cabal run -v0 Coup -- --ws-agents "$CKPT" --human-player 0 --players "$N"
    ;;
  --auto)
    exec cabal run -v0 Coup -- --auto --players "${2:-3}"
    ;;
  *)
    exec cabal run -v0 Coup -- --players "${1:-3}"
    ;;
esac
