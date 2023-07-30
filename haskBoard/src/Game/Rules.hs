module Game.Rules where

import Control.Monad.Free
import Data.Set (Set)
import Game.GameNode (GameAction)
import Game.Location
import Game.Options
import Game.Player (Player)

data GameRuleF l cn r ph pl i next
  = Act (GameAction l cn r ph) next
  | MakeChoice (Options pl i) (pl -> next)
  | LookLocation l (LocationShape r -> next)
  | LookCounter cn (Counter -> next)
  | LookCurrentPhase (ph -> next)
  | LookCurrentTurnOwner (Player -> next)
  | LookPlayers (Set Player -> next)
  deriving (Functor)

type GameRule l cn r ph pl i = Free (GameRuleF l cn r ph pl i)

makeChoice :: Options pl i -> GameRule l cn r ph pl i pl
makeChoice opts = liftF (MakeChoice opts id)

act' :: GameAction l cn r ph -> GameRule l cn r ph pl i ()
act' action = liftF (Act action ())
