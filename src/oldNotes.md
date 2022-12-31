think of C2 l r a as being part of Reader (Game l r) a
so any function Game -> b should be representable as C2 l r b
     ^ that's the 'game logic' abstracted out from the function itself
possibleOptions :: Game -> [Play] =====> C2 l r [Play]
legal :: Game -> Play -> bool =====> C2 l r (Play -> Bool)


What is a Play?
 Computes Transfers from the game state. Like buying a card costs some gems, but
 need the rest of the state to actually compute the Transfer.
 So like state -> [Transfer]

 Plays may have follow-ups from game logic. E.g. discard to hand size or "visit by
 a noble." These should be part of the game logic, not the data of an individual play.

Before working out Play, need to think some about control flow.
Games have Phases. Any sequetial game has phases
data SequentialPhases = PreTurn Player | MainPhase Player | PostTurn Player
Any other time a player needs to make a choice, that's a phase.
data SequentialPhases aux = PreTurn Player | MainPhase Player | PostTurn Player | CustomPhase aux
Really need to just leave this up to game -- will have common patterns, but real danger of overengineering.

Control flow describes:
  - what happens when you enter a phase
  - how a Play moves from phase to phase
But need some memory here.

E.g. a player plays "draw a card, then discard a card." The Card is Transfered to Hand Player.
Phase moves from MainPhase Player to AuxPhaseDiscard Player. The player now has to choose a play.
(Is that a property of phases? Or?)
The only play legal in AuxPhaseDiscard is "discard a card", so the player chooses a card. That card is
Transfered to DiscardPile Player.
Now phase should move back to MainPhase Player.

But if in PostTurn Player, another player has to discard to hand size, then AuxPhaseDiscard has to return
control to PostTurn Player.



When a player choose a play, put it as the root of a tree
Next level is a sequence of transfers, phase changes, and plays.
Each of those may trigger further events.
So we get a tree. In most games it's traversed breadth-first.

When the tree has been completely traversed, the play is resolved.

Games probably shouldn't export this -- use smart constructor
e.g. takeTokens color player = Play ..

For now: just Plays that do transfers or phases!


