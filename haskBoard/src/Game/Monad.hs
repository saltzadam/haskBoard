{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Game.Monad 
    (LookerType(..)
    , GameEff (..)
    , askEff
    , hoistGameEff
    , injectGame
    )
    where
import Game.GameState ( GameState(..),  GameInteract, getGameState )
import GHC.Generics (Generic)
import Control.Applicative (Applicative(..))
import Game.Player (Player)
import Data.Maybe (fromJust)
import Control.Monad.State
import Effectful (Eff, runPureEff, (:>), inject)
import Effectful.Reader.Static (Reader, ask)
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Effectful.Reader.Static as EffR
import qualified Effectful.State.Static.Shared as EffS
import qualified Effectful.State.Static.Shared as State

-- TODO: how many consumers of this would benefit from Maybe-ish typeclasses?
data LookerType = LookAs Player | LookFull deriving (Generic, Eq, Ord, Show)

newtype GameEff l cn r ph pl i a = GameEff {unEff :: MaybeT (Eff '[GameInteract l cn r ph pl i, Reader LookerType]) a}
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

askEff :: GameEff l cn r ph pl i (GameState l cn r ph pl i, LookerType)
askEff = GameEff $ do
    v <- lift ask
    g <- lift getGameState
    return (g, v)

hoistGameEff :: Maybe a -> GameEff l cn r ph pl i a
hoistGameEff = GameEff . MaybeT . pure

runGameEff' :: GameState l cn r ph pl i -> LookerType -> GameEff l cn r ph pl i a -> Maybe a
runGameEff' gs viewer (GameEff (MaybeT uneff)) = runPureEff . EffR.runReader viewer . EffS.evalState gs $ uneff

runGameEff :: GameState l cn r ph pl i -> GameEff l cn r ph pl i a -> a
runGameEff gs eff = fromJust (runGameEff' gs LookFull eff)

injectGame :: (GameInteract l cn r ph pl i :> es) => GameEff l cn r ph pl i a -> Eff es a
injectGame gameEff = let
    f g = (runGameEff g gameEff, g)
                      in inject $ State.state f
