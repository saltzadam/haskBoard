# =============================================================================
# Stage 1: Fetch and build Haskell dependencies (cached layer)
# =============================================================================
FROM haskell:9.10.1 AS haskell-deps
ARG GAME_NAME=NoMerci
WORKDIR /build

RUN apt-get update && apt-get install -y --no-install-recommends \
    zlib1g-dev libgmp-dev pkg-config libncurses-dev git \
    && rm -rf /var/lib/apt/lists/*

# Copy only project config and .cabal files so this layer caches until
# dependencies change (not on every source edit).
COPY cabal.project cabal.project.freeze ./
COPY haskBoard/haskBoard.cabal haskBoard/
COPY Helpers/helpers.cabal Helpers/
COPY Games/${GAME_NAME}/*.cabal Games/${GAME_NAME}/

RUN cabal update && cabal build --only-dependencies all

# =============================================================================
# Stage 2: Compile the game binary
# =============================================================================
FROM haskell-deps AS haskell-build
ARG GAME_NAME=NoMerci

COPY haskBoard/src/ haskBoard/src/
COPY Helpers/ Helpers/
COPY Games/${GAME_NAME}/ Games/${GAME_NAME}/

RUN cabal build ${GAME_NAME} \
    && cp "$(cabal list-bin ${GAME_NAME})" /usr/local/bin/${GAME_NAME} \
    && strip /usr/local/bin/${GAME_NAME}

# =============================================================================
# Stage 3: Final play image (python + numpy + binary + checkpoint)
# =============================================================================
FROM python:3.13-slim AS game
ARG GAME_NAME=NoMerci
ENV GAME_NAME=${GAME_NAME}
ENV PLAYERS=3
ENV HUMAN_PLAYER=0

RUN apt-get update && apt-get install -y --no-install-recommends \
    libgmp10 zlib1g libncursesw6 libtinfo6 \
    && rm -rf /var/lib/apt/lists/*

RUN pip install --no-cache-dir numpy websockets

COPY --from=haskell-build /usr/local/bin/${GAME_NAME} /usr/local/bin/${GAME_NAME}

# Place lite script at the path Haskell expects (python/ws_agent_rllib.py)
COPY python/ws_agent_lite.py /app/python/ws_agent_rllib.py

ARG CHECKPOINT_DIR=python/runs/${GAME_NAME}_default_3/rllib_checkpoints
COPY ${CHECKPOINT_DIR} /app/checkpoint/

COPY docker/entrypoint-play.sh /app/entrypoint.sh
RUN chmod +x /app/entrypoint.sh

ENV TERM=xterm-256color
ENV HASKBOARD_PYTHON_CMD=python

ENTRYPOINT ["/app/entrypoint.sh"]
