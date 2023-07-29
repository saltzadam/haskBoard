module Game.GameNode where

import GHC.Generics (Generic)
import Game.Options
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
  | MakeVisibleTo Player (VisData l cn ph)
  | MakeInvisibleTo Player (VisData l cn ph)
  | EndGame [Player]
  deriving (Eq, Ord, Show, Generic)

newtype GameNode l cn r ph pl i = GameNode
  { node :: Either (GameAction l cn r ph) (Options pl i)
  }
  deriving (Generic, Show)

action :: GameAction l cn r ph -> GameNode l cn r ph pl i
action = GameNode . Left

choice :: Options pl i -> GameNode l cn r ph pl i
choice = GameNode . Right

mkChoice :: Options pl i -> [GameNode l cn r ph pl i]
mkChoice opts = [choice opts]

mkAction :: GameAction l cn r ph -> [GameNode l cn r ph pl i]
mkAction act = [action act]
