module Game.Rules where

import Control.Lens (view)
import Control.Monad (void)
import Control.Monad.Free
import Data.Set (Set)
import GHC.Generics (Generic)
import Game.GameAction
import Game.GameStateBase (GameState)
import Game.Location
import Game.Options
import Game.Player (Player)

data GameRuleF l cn r ph pl next
  = Act (GameAction l cn r ph) next
  | MakeChoice (Options pl) (pl -> next)
  | LookLocation l (LocationShape r -> next)
  | LookCounter cn (Counter -> next)
  | LookCurrentPhase (ph -> next)
  | LookCurrentTurnOwner (Player -> next)
  | LookPlayers (Set Player -> next)
  | LookGameState (GameState l cn r ph pl -> next)
  deriving (Functor)

-- type GameRule l cn r ph pl = Free (GameRuleF l cn r ph pl)

newtype GameRule l cn r ph pl a = GameRule {unRule :: Free (GameRuleF l cn r ph pl) a}
  deriving (Functor, Applicative, Monad, MonadFree (GameRuleF l cn r ph pl), Generic)

instance (Num a) => Num (GameRule l cn r ph pl a) where
  fromInteger = pure . fromInteger
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum

makeChoice :: Options pl -> GameRule l cn r ph pl pl
makeChoice opts = liftF (MakeChoice opts id)

makeChoice_ :: Options a -> GameRule l cn r ph a ()
makeChoice_ = void . makeChoice

act :: GameAction l cn r ph -> GameRule l cn r ph pl ()
act action = liftF (Act action ())

lookPlayers :: GameRule l cn r ph pl (Set Player)
lookPlayers = liftF (LookPlayers id)

lookLocation :: (Eq l) => l -> GameRule l cn r ph pl (LocationShape r)
lookLocation l = liftF (LookLocation l id)

lookCounter :: cn -> GameRule l cn r ph pl Counter
lookCounter c = liftF (LookCounter c id)

lookCounterVal :: cn -> GameRule l cn r ph pl Int
lookCounterVal c = liftF (LookCounter c (view #val))

lookCounterBounds :: cn -> GameRule l cn r ph pl (Int, Int)
lookCounterBounds c = liftF (LookCounter c (view #bounds))

lookCurrentPhase :: GameRule l cn r ph pl ph
lookCurrentPhase = liftF (LookCurrentPhase id)

lookCurrentTurnOwner :: GameRule l cn r ph pl Player
-- lookCurrentTurnOwner = (\(Turn p _) -> p) . view #currentTurn
lookCurrentTurnOwner = liftF (LookCurrentTurnOwner id)

lookGameState :: GameRule l cn r ph pl (GameState l cn r ph pl)
lookGameState = liftF (LookGameState id)
