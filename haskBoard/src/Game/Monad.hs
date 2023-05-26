{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE RankNTypes #-}
module Game.Monad where
import Game.GameState (Game (..), GameState(..), ObserveGame, GameInteract, getGameState)
import GHC.Generics (Generic)
import Control.Applicative (Applicative(..))
import Control.Lens (view, Getting)
import Game.Visibility (VisibilityMap(..), VisibilityType (..), VisData)
import Game.Player (Player)
import Data.Maybe (fromJust)
import Control.Monad.State
import Effectful (Eff, runPureEff)
import Effectful.Reader.Static (Reader(..), ask)
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Effectful.Reader.Static as EffR
import qualified Effectful.State.Static.Shared as EffS


-- newtype GameM l cn r ph pl i a = GameM {unGame :: State (GameState l cn r ph pl i) a}
--     deriving (Generic, Functor, Applicative, Monad)


-- asksGameState :: (GameState l cn r ph pl i -> b) -> GameM l cn r ph pl i b
-- asksGameState f = GameM (state $ \s -> (f s, s))

-- askGameState :: GameM l cn r ph pl i (GameState l cn r ph pl i)
-- askGameState = asksGameState id

-- viewGameState :: Getting b (GameState l cn r ph pl i) b -> GameM l cn r ph pl i b
-- viewGameState = asksGameState . view


-- instance Semigroup a => Semigroup (GameM l cn r ph pl i a) where
--     (<>) = liftA2 (<>)

-- instance Monoid a => Monoid (GameM l cn r ph pl i a) where
--     mempty = pure mempty

-- instance Num a => Num (GameM l cn r ph pl i a) where
--     (+) = liftA2 (+)
--     (*) = liftA2 (*)
--     negate = fmap negate
--     abs = fmap abs
--     signum = fmap signum
--     fromInteger = pure . fromInteger



-- newtype GameView l cn r ph pl i a = GameView {unGame :: StateT (GameState l cn r ph pl i, ViewerType) Maybe a}
--     deriving (Generic, Functor, Applicative, Monad)

-- instance Semigroup a => Semigroup (GameView l cn r ph pl i a) where
--     (<>) = liftA2 (<>)

-- instance Monoid a => Monoid (GameView l cn r ph pl i a) where
--     mempty = pure mempty


-- instance Num a => Num (GameView l cn r ph pl i a) where
--     (+) = liftA2 (+)
--     (*) = liftA2 (*)
--     negate = fmap negate
--     abs = fmap abs
--     signum = fmap signum
--     fromInteger = pure . fromInteger

-- fromViewFull :: GameView l cn r ph pl i a -> GameM l cn r ph pl i a
-- fromViewFull (GameView r) = GameM (state . go . runStateT $ r) where
--     go :: ((GameState l cn r ph pl i, ViewerType) -> Maybe (a, (GameState l cn r ph pl i, ViewerType))) 
--              -> GameState l cn r ph pl i -> (a, GameState l cn r ph pl i )
--     go f g = fmap fst . fromJust . f $ (g, ViewFull)

-- viewToFullState :: GameView l cn r ph pl i a -> (GameState l cn r ph pl i -> a)
-- viewToFullState gv = let 
--     (GameM gv') =  fromViewFull gv
--                 in 
--                   evalState gv'

-- askMM :: GameView l cn r ph pl i (GameState l cn r ph pl i, ViewerType)
-- askMM = GameView (state (\a -> (a,a)))

data ViewerType = ViewAs Player | ViewFull deriving (Generic, Eq, Ord, Show)

-- runVis :: VisibilityMap l cn ph -> ViewerType -> VisData l cn ph -> VisibilityType
-- runVis _ ViewFull _ = Visible
-- runVis (VisibilityMap vis) (ViewAs p) lc = vis p lc

newtype GameEff l cn r ph pl i a = GameEff {unEff :: MaybeT (Eff '[GameInteract l cn r ph pl i, Reader ViewerType]) a}
    deriving (Generic, Functor, Applicative, Monad)

instance Num a => Num (GameEff l cn r ph pl i a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Semigroup a => Semigroup (GameEff l cn r ph pl i a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (GameEff l cn r ph pl i a) where
    mempty = pure mempty

askEff :: GameEff l cn r ph pl i (GameState l cn r ph pl i, ViewerType)
askEff = GameEff $ do
    v <- lift ask
    g <- lift getGameState
    return (g, v)

hoistGameEff :: Maybe a -> GameEff l cn r ph pl i a
hoistGameEff = GameEff . MaybeT . pure

runGameEff' :: GameState l cn r ph pl i -> ViewerType -> GameEff l cn r ph pl i a -> Maybe a
runGameEff' gs viewer (GameEff (MaybeT uneff)) = runPureEff . EffR.runReader viewer . EffS.evalState gs $ uneff

runGameEff :: GameState l cn r ph pl i -> GameEff l cn r ph pl i a -> a
runGameEff gs eff = fromJust (runGameEff' gs ViewFull eff)
