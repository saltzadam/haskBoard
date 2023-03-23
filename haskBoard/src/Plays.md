Hierarchy:
- Play: choices by user, highest level of abstraction
- Move: lowest level of abstraction for users
- Actions: lowest level of the interface

Technically, the hierarchy makes sense in reverse:
- Actions: GameAction 
- Move: Eff es [GameAction]
    - A collection of Actions computed from the GameState. They are computed all at once: Actions are computed before knowing what `act action` will do. (Of course the computation could be clever.)

    What we're trying to avoid: suppose we want to advance a piece
    twice. Then Eff es [advancePiece, advancePiece] will expand to
    [Transfer piece oldLoc newLoc, Transfer piece oldLoc newLoc]. So
    actually the piece will only advance twice. The right idea is to
    have `advancePiece = Eff es (some computation)` and then
    `advanceTwice = advancePiece >> advancePiece`.

- Play

Can't Stop:
- Plays: move these dice, stop
- Moves: move piece to track, advance piece, remove piece

Splendor:
- Plays: buy, reserve, etc
- Moves: place card, pay cost
  - thinner!

QM1914
- Plays: play card and make choices, prepare card
- Moves: battle? place army/fleet at _
  - so many cascading choices!





