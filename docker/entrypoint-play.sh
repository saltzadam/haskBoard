#!/bin/bash
set -e
PLAYERS="${PLAYERS:-3}"
HUMAN="${HUMAN_PLAYER:-0}"
GAME="${GAME_NAME:-NoMerci}"
cd /app
exec "$GAME" --ws-agents /app/checkpoint --human-player "$HUMAN" --players "$PLAYERS"
