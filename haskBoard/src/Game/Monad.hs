{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
module Game.Monad where
import Game.GameState (Game (..), GameState(..))
import GHC.Generics (Generic)
import Control.Monad.Trans.Reader (Reader)
import Control.Monad.Reader (MonadReader (..), asks, runReader, withReader)
import Control.Applicative (Applicative(..))
import Control.Lens (view, Getting, to)
import Game.Visibility (VisibilityMap(..), VisibilityType (..))

newtype GameM l cn r ph pl i a = GameM {unGame :: Reader (Game l cn r ph pl i) a} 
    deriving (Generic, Functor, Applicative, Monad, MonadReader (Game l cn r ph pl i))

zoomOutState' :: Reader (GameState l cn r ph pl i) a -> Reader (Game l cn r ph pl i) a
zoomOutState' = withReader (view #gameState)

-- TODO: Game is probably a monoid lol
liftDumb :: GameState l cn r ph pl i -> Game l cn r ph pl i
liftDumb gs = Game gs 
                (\_ _ -> [])
                (VisibilityMap (\_ _ -> Invisible) ) 
                (const [])

zoomInState' :: Reader (Game l cn r ph pl i) a -> Reader (GameState l cn r ph pl i) a
zoomInState' = withReader liftDumb

zoomInState :: GameM l cn r ph pl i a -> Reader (GameState l cn r ph pl i) a
zoomInState = zoomInState' . unGame

asksGame :: (Game l cn r ph pl i -> b) -> GameM l cn r ph pl i b
asksGame = asks

askGame :: GameM l cn r ph pl i (Game l cn r ph pl i)
askGame = asks id

useGame :: Getting b (Game l cn r ph pl i) b -> GameM l cn r ph pl i b
useGame = asks . view

asksGameState :: (GameState l cn r ph pl i -> b) -> GameM l cn r ph pl i b
asksGameState f = asks (f . view #gameState)

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
                                        


