# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
# Build all packages
cabal build all

# Build a specific package
cabal build haskBoard
cabal build NoMerci

# Run a game
cabal run NoMerci

# Run tests (currently commented out in cabal; re-enable in haskBoard/haskBoard.cabal to use)
cabal test haskboard-test
```

GHC version: 9.6.x (see `dist-newstyle/` for exact version). The cabal cradle (`hie.yaml`) is minimal — just `cradle: cabal:`.

## Architecture

haskboard is a Haskell framework for implementing turn-based board games. It separates game logic, state, and interfaces cleanly.

### Core type parameters

Throughout the codebase, types are parameterized by:
- `l` — location name (e.g. `NMLocation`: `CardDeck`, `PlayerStuff`)
- `cn` — counter name
- `r` — resource/piece type (e.g. `NMCard`, `Chip`)
- `ph` — phase name (e.g. `NMPhaseName`)
- `pl` — play/move type (e.g. `NMPlayName`: `Take`, `Decline`)

All of `l`, `cn`, `r` must be `Finitary` (from the `finitary` package) — they are finite enumerable types used as map keys in `FTMap`.

### Game state (`Game.GameState`, `Game.Location`)

`GameState l cn r ph pl` holds:
- `objects :: GameObjects l cn r` — contains `Locations` (a `FTMap` of `LocationShape r`) and `Counters` (a `FTMap` of `Counter`)
- `players`, `currentPhase`, `currentTurn`, `nextTurn`, `visibility`

`LocationShape r` is the key data type for where resources live:
- `Deck` — ordered sequence (transfers from/to top)
- `Pile` — unordered multiset
- `Slot` — holds 0 or 1 item
- `Infinite` — unbounded supply
- `Dummy` — no-op

### Game rules DSL (`Game.Rules`, `Game.GameAction`)

Games are defined as `GameRule l cn r ph pl a` — a free monad over `GameRuleF`. Game logic is written using combinators like `act`, `lookLocation`, `lookCounter`, `makeChoice`, `lookCurrentTurnOwner`, etc.

`GameAction` is the set of all primitive game mutations: transfers between locations, counter operations, shuffle, visibility changes, turn/phase control, `EndGame`.

### Effectful execution (`Game.GameE`)

`playGameTurns` runs the game using the `effectful` library (not MTL). The effect stack is:
- `GameInteract` = `State (GameState ...)` — mutable game state
- `GameRun` = `Reader (GameRules ...)` — read-only rules
- `Interface l cn r ph pl` — dynamic effect for player interaction (choose, update, announce)
- `RNG` (CryptoRNG) — randomness
- `Log2` — logging to file

`runRuleControl` interprets `GameRule` free monad nodes into effectful actions.

### Interfaces and agents (`Interface.Controller`, `Interface.Agent`, `Interface.Server`)

The `Interface` effect is interpreted by `chooseInterface` using a `GameController`, which maps each `Player` to a `PlayerInterface` (two `Chan`s: `fromGame` and `toGame`).

Agent types:
- `randomAgent` — picks random legal moves (AI)
- `brickAgent` — bridges game channels to Brick `BChan`s for TUI
- `termAgent` — reads/writes to terminal

`Interface.Server` runs a WebSocket server on `127.0.0.1:9159`. Players connect, send their `PlayerNum`, and the server relays `GameToInterfacePayload` as JSON.

### Views and visibility (`Game.View`, `Game.Visibility`)

`GameStateView` is a filtered view of game state for a specific player — hidden locations/counters appear as `Nothing`. `viewGameStateAs` / `viewGameStateAs'` produce these views. The `VisibilityMap` tracks what each player can see.

### Brick TUI (`Brick.Game.Tui`)

`TUIState` and composable `TUIEventHandler`s (a `Monoid`) handle Brick events. `basicHandler` covers state updates, options requests, winner announcements, and ESC. `simpleHandler` adds numeric key selection. Games provide their own `app` using these primitives.

### Implementing a game

See `Games/NoMerci/` as the reference implementation:
1. `Objects.hs` — define `l`, `cn`, `r`, `ph`, `pl` types + initial `GameObjects`
2. `NoMerci.hs` — define phases, play runner, scoring, initial state; export `noMerci :: Int -> (NMGameState, NMGameRules)`
3. `Tui.hs` — define a Brick `app` using `Brick.Game.Tui` helpers
4. `Main.hs` — wire together with `buildInterface`, `runGameSeparateChannels`, and `server`

`Helpers/Helpers.hs` provides game-writing utilities (`transfer`, `draw`, `advanceTurn`, `endGame`, `activePlayer`, etc.) that wrap `Game.Rules` combinators.
