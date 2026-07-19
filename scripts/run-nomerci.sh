#!/bin/bash
# Launch NoMerci ("No Thanks!") locally: you play in a TUI vs. AI agents backed
# by a trained RLlib checkpoint.
#
# The AI opponents are served by python/ws_agent_rllib.py, which `uv` runs
# against the python/ project (torch + ray[rllib] + gymnasium). With
# HASKBOARD_PYTHON_CMD unset, the game invokes:
#     uv run --project python python python/ws_agent_rllib.py --checkpoint ... --player N
#
# Requirements:
#   * uv on PATH (brew install uv). First run provisions the ML stack (large).
#   * macOS arm64: python/pyproject.toml pins torch to a CUDA index for
#     non-Linux (pytorch-cu130), which has no macOS wheels. Point it at a
#     CPU/MPS index (e.g. the default PyPI torch) if `uv` fails to resolve.
#
# Usage:
#   ./run-nomerci.sh [PLAYERS] [HUMAN_PLAYER]
#   ./run-nomerci.sh           # 3 players, you are player 0
#   ./run-nomerci.sh 5 2       # 5 players, you are player 2
set -e
cd "$(dirname "$0")"

PLAYERS="${1:-3}"
HUMAN="${2:-0}"

if [ "$PLAYERS" -lt 3 ] || [ "$PLAYERS" -gt 5 ]; then
  echo "PLAYERS must be 3-5 (got $PLAYERS)"; exit 1
fi

if ! command -v uv >/dev/null 2>&1; then
  echo "Error: 'uv' is required to run the RLlib agent (brew install uv)." >&2
  echo "See this script's header for the macOS torch caveat." >&2
  exit 1
fi

export PATH="$HOME/.ghcup/bin:$PATH"
# HASKBOARD_PYTHON_CMD intentionally unset so the game uses the default
# `uv run --project python ...` invocation against the real agent.

CHECKPOINT="python/runs/default_lrem5_${PLAYERS}/rllib_checkpoints"
if [ ! -d "$CHECKPOINT" ]; then
  echo "No checkpoint for $PLAYERS players at $CHECKPOINT"; exit 1
fi

mkdir -p logs
exec cabal run -v0 NoMerci -- \
  --ws-agents "$CHECKPOINT" \
  --human-player "$HUMAN" \
  --players "$PLAYERS"
