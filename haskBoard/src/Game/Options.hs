{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module Game.Options where

import Control.Lens (makeFields, view)
import Control.Monad (join)
import qualified Data.Foldable as F
import Data.Generics.Labels ()
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics (Generic)
import Game.Player
import Util (buildSafeNonempty, graph, graphM)

-- A move may have lots of illegality.
-- Concatenate where possible.

data Legality illegal = Legal | Illegal (NE.NonEmpty illegal) deriving (Eq, Ord, Show, Generic, Functor)

instance Semigroup (Legality i) where
  Legal <> x = x
  x <> Legal = x
  Illegal x <> Illegal y = Illegal (x <> y)

firstLegal :: Eq i => (pl -> Legality i) -> [pl] -> Maybe pl
firstLegal check (play : plays) =
  if check play == Legal
    then Just play
    else firstLegal check plays
firstLegal _ [] = Nothing

firstLegalM :: (Eq illegal, Monad m) => (b -> m (Legality illegal)) -> [b] -> m (Maybe b)
firstLegalM check (play : plays) = do
  checkM <- check play
  if checkM == Legal
    then return $ Just play
    else firstLegalM check plays
firstLegalM _ [] = return Nothing

instance Monoid (Legality i) where
  mempty = Legal

data Options pl i = Options
  { legal :: NonEmpty pl,
    illegal :: Map pl (Legality i),
    owner :: Player
  }
  deriving (Eq, Ord, Show, Generic)


makeFields ''Options

t :: Options pl i -> Player
t = view #owner

-- opts0 <> opts1 is opts0 overridden w/ opts1
instance (Ord pl, Eq i) => Semigroup (Options pl i) where
  (Options legal illegal owner) <> (Options legal' illegal' _) = let
    asMap  = M.fromList [(play, Legal) | play <- NE.toList legal] <> illegal
    asMap' = M.fromList [(play, Legal) | play <- NE.toList legal'] <> illegal'
    newMap = asMap <> asMap'
    (newLegal, newIllegal) = M.partition (== Legal) newMap
    in Options (NE.fromList (M.keys newLegal)) newIllegal owner -- unsafe NE.fromList but newLegal is nonempty

raiseIssueIf :: issue -> Bool -> Legality issue
raiseIssueIf = flip mustNotElse

mustElse :: Bool -> issue -> Legality issue
mustElse True _ = Legal
mustElse False i = Illegal [i]

mustNotElse :: Bool -> issue -> Legality issue
mustNotElse True i = Illegal [i]
mustNotElse False _ = Legal

buildOptions' :: (Eq issue, Ord play, Foldable t) => Player -> play -> t (play, Legality issue) -> Options play issue
buildOptions' p defaultPlay playMap =
  let (legalMoves, illegalMoves) = M.partition (== Legal) . M.fromList . F.toList $ playMap
   in Options
        (buildSafeNonempty (M.keys legalMoves) defaultPlay)
        illegalMoves
        p

buildOptions'' :: Eq i => Player -> pl -> Map pl (Legality i) -> Options pl i
buildOptions'' p defaultPlay playMap =
  let (legalMoves, illegalMoves) = M.partition (== Legal) $ playMap
   in Options
        (buildSafeNonempty (M.keys legalMoves) defaultPlay)
        illegalMoves
        p

buildOptions :: (Monad m, Traversable t, Eq issue, Ord play, Foldable m) => Player -> (play -> m (Legality issue)) -> play -> m (t play) -> m (Options play issue)
buildOptions p checkPlay defaultPlay plays =
  let playsSet = S.fromList . F.toList <$> plays
      playsMap = (sequence . M.fromSet checkPlay) =<< playsSet
   in buildOptions'' p defaultPlay <$> playsMap

displayOptions :: (Show pl, Show i) => Options pl i -> String
displayOptions (Options legalO illegalO p) =
  show (NE.toList legalO)
    ++ "\n"
    ++ "Cannot choose: "
    ++ showMapAsList illegalO
  where
    showMapAsList :: (Show pl, Show i) => Map pl (Legality i) -> String
    showMapAsList = show . M.toList
