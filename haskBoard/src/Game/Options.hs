{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
module Game.Options where
import GHC.Generics (Generic)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Control.Lens (makeFields)
import Data.Map (Map)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Foldable as F
import Util (graphM)
import qualified Data.Text as T
import Data.Text (Text)

-- A move may have lots of illegality.
-- Concatenate where possible.

data Legality illegal = Legal | Illegal [illegal] deriving (Eq, Ord, Show, Generic, Functor)

instance Semigroup (Legality i) where
    Legal <> x = x
    x <> Legal = x
    Illegal x <> Illegal y = Illegal (x <> y)


instance Monoid (Legality i) where
    mempty = Legal

buildSafeNonempty :: [a] -> a -> NonEmpty a
buildSafeNonempty xs def = if null xs then def :| [] else NE.fromList xs

data Options pl i = Options {legal :: NonEmpty pl,
                             illegal :: Map pl (Legality i)
                            } deriving (Eq, Ord, Show, Generic)


makeFields ''Options

mustElse :: Bool -> issue -> Legality issue
mustElse True _ = Legal
mustElse False i = Illegal [i]

mustNotElse :: Bool -> issue -> Legality issue
mustNotElse True i = Illegal [i]
mustNotElse False _ = Legal

buildOptions :: (Monad m, Traversable t, Eq issue, Ord play) => (play -> m (Legality issue)) -> play -> m (t play) -> m (Options play issue)
buildOptions checkPlay defaultPlay plays = do
    thePlays <- plays
    legalities <- traverse (graphM checkPlay) thePlays
    let (legalMoves, illegalMoves) = M.partition (== Legal) . M.fromList . F.toList $ legalities
    let legalMovesWithDefault = buildSafeNonempty (M.keys legalMoves) defaultPlay    
    return (Options legalMovesWithDefault illegalMoves)

displayOptions :: (Show pl, Show i) => Options pl i -> String
displayOptions (Options legalO illegalO) =
    show (NE.toList legalO) ++ "\n"
    ++ "Cannot choose: " ++ showMapAsList illegalO
        where
            showMapAsList :: (Show pl, Show i) => Map pl (Legality i) -> String
            showMapAsList = show . M.toList
