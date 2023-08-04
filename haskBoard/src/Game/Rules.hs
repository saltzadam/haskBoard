module Game.Rules where

import Control.Applicative
import Control.Lens (view)
import Control.Monad.Free
import Data.Set (Set)
import GHC.Generics (Generic)
import Game.GameAction
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

-- type GameRule l cn r ph pl i = Free (GameRuleF l cn r ph pl i)

newtype GameRule l cn r ph pl i a = GameRule {unRule :: Free (GameRuleF l cn r ph pl i) a}
  deriving (Functor, Applicative, Monad, MonadFree (GameRuleF l cn r ph pl i), Generic)

instance Num a => Num (GameRule l cn r ph pl i a) where
  fromInteger = pure . fromInteger
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum

makeChoice :: Options pl i -> GameRule l cn r ph pl i pl
makeChoice opts = liftF (MakeChoice opts id)

act :: GameAction l cn r ph -> GameRule l cn r ph pl i ()
act action = liftF (Act action ())

lookPlayers :: GameRule l cn r ph pl i (Set Player)
lookPlayers = liftF (LookPlayers id)

lookLocation :: Eq l => l -> GameRule l cn r ph pl i (LocationShape r)
lookLocation l = liftF (LookLocation l id)

lookCounter :: cn -> GameRule l cn r ph pl i Counter
lookCounter c = liftF (LookCounter c id)

lookCounterVal :: cn -> GameRule l cn r ph pl i Int
lookCounterVal c = liftF (LookCounter c (view #val))

lookCounterBounds :: cn -> GameRule l cn r ph pl i (Int, Int)
lookCounterBounds c = liftF (LookCounter c (view #bounds))

lookCurrentPhase :: GameRule l cn r ph pl i ph
lookCurrentPhase = liftF (LookCurrentPhase id)

lookCurrentTurnOwner :: GameRule l cn r ph pl i Player
-- lookCurrentTurnOwner = (\(Turn p _) -> p) . view #currentTurn
lookCurrentTurnOwner = liftF (LookCurrentTurnOwner id)
