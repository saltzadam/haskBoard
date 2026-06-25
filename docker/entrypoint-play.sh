#!/bin/bash
set -e
PLAYERS="${PLAYERS:-3}"
HUMAN="${HUMAN_PLAYER:-0}"
cd /app
exec NoMerci --ws-agents /app/checkpoint --human-player "$HUMAN" --players "$PLAYERS"
