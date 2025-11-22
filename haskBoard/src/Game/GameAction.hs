module Game.GameAction (GameAction (..)) where

import Data.Text
import GHC.Generics (Generic)
import Game.Player
import Game.Visibility (VisData)

-- These are the fundamental actions in a game. All the "verbs" of a game (besides the observations, e.g. "check" and "count") can be phrased in terms of these.
data GameAction l cn r ph
  = DoNothing
  | MkTransfer l l r
  | MkSwap l l r r
  | IncrementCounter cn
  | DecrementCounter cn
  | TransferCounter cn cn
  | SetCounter cn Int
  | RollCounter cn
  | -- | AddCounter cn
    -- | RemoveCounter cn
    Shuffle l
  | -- | ChangePhase ph
    EndPhase
  | AdvanceTurn
  | SetNextTurn (Maybe (Turn ph))
  | MakeVisibleTo Player (VisData l cn ph)
  | MakeInvisibleTo Player (VisData l cn ph)
  | EndGame [Player]
  | MakeAnnouncement (Maybe Player) Text
  deriving (Eq, Ord, Show, Generic)
