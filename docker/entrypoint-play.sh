#!/bin/bash
set -e
PLAYERS="${PLAYERS:-3}"
HUMAN="${HUMAN_PLAYER:-0}"
GAME="${GAME_NAME:-NoMerci}"

if [ "$PLAYERS" -lt 3 ] || [ "$PLAYERS" -gt 5 ]; then
  echo "Error: PLAYERS must be between 3 and 5 (got $PLAYERS)" >&2
  exit 1
fi

if [ "$HUMAN" -lt 0 ] || [ "$HUMAN" -ge "$PLAYERS" ]; then
  echo "Error: HUMAN_PLAYER must be between 0 and $((PLAYERS - 1)) (got $HUMAN)" >&2
  exit 1
fi

CHECKPOINT="/app/checkpoints/${PLAYERS}"
if [ ! -d "$CHECKPOINT" ]; then
  echo "Error: No checkpoint found for $PLAYERS players at $CHECKPOINT" >&2
  exit 1
fi

cd /app
exec "$GAME" --ws-agents "$CHECKPOINT" --human-player "$HUMAN" --players "$PLAYERS"
