# Coup review TODOs

## Logic
1. [x] Assassinate/Steal resolve against a target already eliminated by a caught block bluff (duplicate "assassinated"/"eliminated" announcements; steal from a dead player).
2. [x] `Reveal Role` reused for Exchange returns — wrong TUI prompt, ambiguous RL action. Add `ReturnCard Role`.
3. [x] Assassin's 3 coins not refunded when the Assassin claim is successfully challenged — correct per rules; no change.
4. [x] `advanceToNextAlive` `[]` branch is unreachable; document it.
5. [x] No turn cap — per-step penalty + `max_steps` truncation in the Python env.

## Types / aliases
6. [x] `CoupTurn` is both a type alias and a `CoupPhaseName` constructor; rename the constructor.
7. [x] Remove dead `isRoleCard`/`extractPlayer`; `extractRole` → total `roleOf`; move `CoupGameObjects` into the alias block.
8. [x] `ExchangeZone` unused but in the observation — use it for Exchange or remove it.
9. [x] Cabal: duplicate extensions, `-threaded` on library, no `-Wall` on executable.

## Helpers
10. [x] Add `incrementCounter`/`decrementCounter`/`transferCounter`/`announceBy` to Helpers; drop `Game.GameAction` import from Coup; `stealCoins` uses `transferCounter`.
11. [x] `influenceRoles` → `whatsAt`; `influenceCount` → `resourcesAt`.
12. [x] Add `nextPlayerAmong` (current player need not be eligible) to Helpers; use in `advanceToNextAlive`.
13. [x] `OverloadedStrings` + `tshow` instead of `tpack`/`T.pack` mix.

## Brick / Main
14. [x] `ShowState` says "<turn owner> is deciding..." during others' reactions; use `viewCurrentPlayer`.
15. [x] Rename `influenceDesc`/`revealedDesc` → `printInfluence`/`printRevealed`.
16. [x] Add `viewWhatsAt` to Helpers; replace `rolesAt`.
17. [x] `lastActionOf` strips player names from announcement text — fragile coupling to message wording.
18. [x] Main: reject out-of-range `--players`; `readMaybe` for `--human-player`; drop redundant `FlexibleContexts`.

## Follow-ups outside Coup
19. [ ] Audit the other games' cabal files (NoMerci, LoveLetter, CantStop) the same way: duplicate extensions, `-threaded` on libraries, `-Wall` on executables, unused `build-depends` (`-Wunused-packages`), unneeded `Effectful.Plugin`.
20. [ ] `Game.Location.inventoryItems` (`M.keys . inventory`) lists zero-count entries left in a `Pile` after its last copy of a resource leaves. Harmless in NoMerci today (only chips leave `PlayerStuff`, and they're filtered out); consider filtering `> 0` in the library.
21. [ ] Add local-vs-random and headless-random modes to `Run.Game.RunMode`; replace the hand-rolled harnesses in Coup (`tuiMain`/`autoMain`), CantStop and LoveLetter.
22. [ ] NoMerci `Main.hs`: `maybe 0 read` on `--human-player` crashes on bad input; use `readMaybe`.
