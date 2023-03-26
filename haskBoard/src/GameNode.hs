{-# LANGUAGE DeriveGeneric #-}
module GameNode 
    where
import GHC.Generics (Generic)
import Count
import Game.Options
import Game.Player
    --
-- These are the fundamental actions in a game. All the "verbs" of a game (besides the observations, e.g. "check" and "count") can be phrased in terms of these.IO 
data GameAction l cn r ph
  = DoNothing
  | MkTransfer l l r
  | IncrementCounter cn
  | DecrementCounter cn
  | SetCounter cn (Cnt Int)
  | RollCounter cn
  | ChangePhase ph
  | EndGame
  deriving (Eq, Ord, Show, Generic)


data GameNode l cn r ph pl i = GameNode
  { node :: Either  (GameAction l cn r ph) (Options pl i),
    owner :: Maybe Player
  }
  deriving (Generic, Show)


mkActionNode :: GameAction l cn r ph -> GameNode l cn r ph pl i
mkActionNode action = GameNode (Left action) Nothing

mkGetOptionsNode :: Player -> Options pl i -> GameNode l cn r ph pl i
mkGetOptionsNode p choice = GameNode (Right choice) (Just p)


