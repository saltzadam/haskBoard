#!/bin/bash
# Play Coup locally: you drive Player 1 in a Brick TUI; the other seats are
# random-move AI. No trained checkpoint needed.
#
# Usage:
#   ./run-coup.sh [PLAYERS]        # play (default 3 players, 2-6)
#   ./run-coup.sh --auto [PLAYERS] # headless self-play, prints the winner
set -e
cd "$(dirname "$0")"
export PATH="$HOME/.ghcup/bin:$PATH"

if [ "$1" = "--auto" ]; then
  exec cabal run -v0 Coup -- --auto --players "${2:-3}"
else
  exec cabal run -v0 Coup -- --players "${1:-3}"
fi
