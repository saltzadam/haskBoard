{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Game.Monad where
import Game.GameState (Game (..), GameState(..))
import GHC.Generics (Generic)
import Control.Monad.Trans.Reader (Reader, ReaderT(..))
import Control.Monad.Reader (MonadReader (..), asks, withReader, runReader)
import Control.Applicative (Applicative(..))
import Control.Lens (view, Getting)
import Game.Visibility (VisibilityMap(..), VisibilityType (..))
import Game.Player (Player)
import Data.Maybe (fromJust)

newtype GameM l cn r ph pl i a = GameM {unGame :: Reader (GameState l cn r ph pl i) a}
    deriving (Generic, Functor, Applicative, Monad, MonadReader (GameState l cn r ph pl i))

-- zoomOutState' :: Reader (GameState l cn r ph pl i) a -> Reader (Game l cn r ph pl i) a
-- zoomOutState' = withReader (view #gameState)

-- -- TODO: Game is probably a monoid lol
-- liftDumb :: GameState l cn r ph pl i -> Game l cn r ph pl i
-- liftDumb gs = Game gs
--                 (\_ _ -> [])
--                 (const [])

-- zoomInState' :: Reader (Game l cn r ph pl i) a -> Reader (GameState l cn r ph pl i) a
-- zoomInState' = withReader liftDumb

-- zoomInState :: GameM l cn r ph pl i a -> Reader (GameState l cn r ph pl i) a
-- zoomInState (GameM g) = zoomInState' g

-- asksGame :: (Game l cn r ph pl i -> b) -> GameM l cn r ph pl i b
-- asksGame = asks

-- askGame :: GameM l cn r ph pl i (Game l cn r ph pl i)
-- askGame = asks id

-- useGame :: Getting b (Game l cn r ph pl i) b -> GameM l cn r ph pl i b
-- useGame = asks . view

asksGameState :: (GameState l cn r ph pl i -> b) -> GameM l cn r ph pl i b
asksGameState = asks

askGameState :: GameM l cn r ph pl i (GameState l cn r ph pl i)
askGameState = asksGameState id

viewGameState :: Getting b (GameState l cn r ph pl i) b -> GameM l cn r ph pl i b
viewGameState = asksGameState . view


instance Semigroup a => Semigroup (GameM l cn r ph pl i a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (GameM l cn r ph pl i a) where
    mempty = pure mempty

instance Num a => Num (GameM l cn r ph pl i a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger


data ViewerType = ViewAs Player | ViewFull deriving (Generic, Eq, Ord, Show)

newtype GameView l cn r ph pl i a = GameView {unGame :: ReaderT (GameState l cn r ph pl i, ViewerType) Maybe a}
    deriving (Generic, Functor, Applicative, Monad)

instance Semigroup a => Semigroup (GameView l cn r ph pl i a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (GameView l cn r ph pl i a) where
    mempty = pure mempty


instance Num a => Num (GameView l cn r ph pl i a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger



runVis :: VisibilityMap l cn -> ViewerType -> Either l cn -> VisibilityType
runVis _ ViewFull _ = Visible
runVis (VisibilityMap vis) (ViewAs p) lc = vis p lc

askMM :: GameView l cn r ph pl i (GameState l cn r ph pl i, ViewerType)
askMM = GameView (reader id)



fromViewFull :: GameView l cn r ph pl i a -> GameM l cn r ph pl i a
fromViewFull (GameView r) = GameM (reader . go . runReaderT $ r) where
    go ::  ((GameState l cn r ph pl i, ViewerType) -> Maybe a) -> (GameState l cn r ph pl i -> a)
    go f g = fromJust . f $ (g, ViewFull)

viewToFullState :: GameView l cn r ph pl i a -> (GameState l cn r ph pl i -> a)
viewToFullState gv = let 
    (GameM gv') =  fromViewFull gv
                in 
                  runReader gv'
