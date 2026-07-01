# haskBoard

## Docker

### Download

```bash
docker pull ghcr.io/saltzadam/nomerci:main
```

### Build

```bash
docker build -t haskboard .
```

### Run

```bash
docker run -it haskboard
```

| Env var | Default | Description |
|---------|---------|-------------|
| `PLAYERS` | `3` | Number of players (3–5) |
| `HUMAN_PLAYER` | `0` | Which player you control (0 to PLAYERS-1) |

Examples:

```bash
# 5 players, you are player 2
docker run -it -e PLAYERS=5 -e HUMAN_PLAYER=2 haskboard

# 4 players, save game logs to host
docker run -it -e PLAYERS=4 -v ./game-logs:/app/logs haskboard
```
