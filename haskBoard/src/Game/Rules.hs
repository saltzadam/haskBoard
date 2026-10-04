{-# LANGUAGE FunctionalDependencies #-}

module Game.Rules where

import Control.Lens (view, (^.))
import Control.Monad (void)
import Control.Monad.Free
import qualified Data.Set as S
import FinitaryMap (ftAt)
import GHC.Generics (Generic)
import Game.GameAction
import Game.GameStateBase (GameState)
import Game.Location (Counter, LocationShape)
import Game.Options
import Game.Player (Player, Turn (..))

data GameRuleF l cn r ph pl next
  = Act (GameAction l cn r ph) next
  | MakeChoice (Options pl) (pl -> next)
  | LookGameState (GameState l cn r ph pl -> next)
  deriving (Functor)

newtype GameRule l cn r ph pl a = GameRule {unRule :: Free (GameRuleF l cn r ph pl) a}
  deriving (Functor, Applicative, Monad, MonadFree (GameRuleF l cn r ph pl), Generic)

instance (Num a) => Num (GameRule l cn r ph pl a) where
  fromInteger = pure . fromInteger
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum

-- | A read-only computation. It reads the game state but cannot change it or ask players
-- for choices. Scores and hints are queries.
newtype Query l cn r ph pl a = Query (GameState l cn r ph pl -> a)
  deriving (Functor, Applicative, Monad)

runQuery :: Query l cn r ph pl a -> GameState l cn r ph pl -> a
runQuery (Query f) = f

instance (Num a) => Num (Query l cn r ph pl a) where
  fromInteger = pure . fromInteger
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum

-- | Monads that can run read-only queries. 'Query' and 'GameRule' are the two instances.
class (Monad m) => QueryM l cn r ph pl m | m -> l cn r ph pl where
  query :: Query l cn r ph pl a -> m a

instance QueryM l cn r ph pl (Query l cn r ph pl) where
  query = id

instance QueryM l cn r ph pl (GameRule l cn r ph pl) where
  query (Query f) = GameRule (liftF (LookGameState f))

makeChoice :: Options pl -> GameRule l cn r ph pl pl
makeChoice opts = liftF (MakeChoice opts id)

makeChoice_ :: Options a -> GameRule l cn r ph a ()
makeChoice_ = void . makeChoice

act :: GameAction l cn r ph -> GameRule l cn r ph pl ()
act action = liftF (Act action ())

lookGameState :: (QueryM l cn r ph pl m) => m (GameState l cn r ph pl)
lookGameState = query (Query id)

lookLocation :: (QueryM l cn r ph pl m, Eq l) => l -> m (LocationShape r)
lookLocation l = query (Query (view (#objects . #locations . ftAt l)))

lookCounter :: (QueryM l cn r ph pl m, Eq cn) => cn -> m Counter
lookCounter c = query (Query (view (#objects . #counters . ftAt c)))

lookCounterVal :: (QueryM l cn r ph pl m, Eq cn) => cn -> m Int
lookCounterVal c = view #val <$> lookCounter c

lookCounterBounds :: (QueryM l cn r ph pl m, Eq cn) => cn -> m (Int, Int)
lookCounterBounds c = view #bounds <$> lookCounter c

lookCurrentPhase :: (QueryM l cn r ph pl m) => m ph
lookCurrentPhase = query (Query (view #currentPhase))

lookCurrentTurnOwner :: (QueryM l cn r ph pl m) => m Player
lookCurrentTurnOwner = query (Query (\gs -> let Turn p _ = gs ^. #currentTurn in p))

lookPlayers :: (QueryM l cn r ph pl m) => m [Player]
lookPlayers = query (Query (S.toList . view #players))
