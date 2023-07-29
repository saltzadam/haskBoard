{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Options where

import Control.Applicative (liftA3)
import Control.Lens (makeFields)
import qualified Data.Foldable as F
import Data.Generics.Labels ()
import Data.List (delete, partition)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics (Generic)
import Game.Player
import Util (buildSafeNonempty, compose, graphM)

-- A move may have lots of illegality.
-- Concatenate where possible.

data Legality illegal = Legal | Illegal (NE.NonEmpty illegal) deriving (Eq, Ord, Show, Generic, Functor)

oneIssue :: illegal -> Legality illegal
oneIssue i = Illegal (NE.singleton i)

isIllegal :: Legality illegal -> Bool
isIllegal (Illegal _) = True
isIllegal _ = False

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

-- opts0 <> opts1 is opts0 overridden w/ opts1
instance (Ord pl, Eq i) => Semigroup (Options pl i) where
  (Options legal illegal owner) <> (Options legal' illegal' _) =
    let asMap = M.fromList [(play, Legal) | play <- NE.toList legal] <> illegal
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
  let (legalMoves, illegalMoves) = M.partition (== Legal) playMap
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

youMay' :: Player -> NonEmpty pl -> Options pl i
youMay' p basePlays = Options basePlays M.empty p

youMay :: Functor f => Player -> f (NonEmpty pl) -> f (Options pl i)
youMay p mBasePlays = youMay' p <$> mBasePlays

makeIllegal' :: Ord pl => pl -> Legality i -> pl -> Options pl i -> Options pl i
makeIllegal' play issue def (Options legal illegal p) =
  Options
    (buildSafeNonempty (delete play (NE.toList legal)) def)
    (M.insert play issue illegal)
    p

exceptIfMap' :: Ord pl => Map pl (Legality i) -> pl -> Options pl i -> Options pl i
exceptIfMap' issueMap def opts =
  let issueTuples = M.toList issueMap
      makeAllIllegal = compose (($ def) . uncurry makeIllegal' <$> issueTuples)
   in makeAllIllegal opts

exceptIfMap :: (Applicative m, Ord pl) => m (Map pl (Legality i)) -> m pl -> m (Options pl i) -> m (Options pl i)
exceptIfMap = liftA3 exceptIfMap'

exceptIf' :: (Ord pl, Eq i) => (pl -> Legality i) -> pl -> Options pl i -> Options pl i
exceptIf' issuef def (Options legal illegal p) =
  let (stillLegal, newlyIllegal) = partition ((== Legal) . issuef) (NE.toList legal)
   in Options
        (buildSafeNonempty stillLegal def)
        (M.fromSet issuef (S.fromList newlyIllegal) <> illegal)
        p

exceptIf :: (Ord pl, Eq i, Monad m) => (pl -> m (Legality i)) -> m pl -> m (Options pl i) -> m (Options pl i)
exceptIf issuefm mdef mopts = do
  def <- mdef
  (Options legal illegal p) <- mopts
  legalEval <- traverse (graphM issuefm) legal
  let (stillLegal, newlyIllegal) = partition ((== Legal) . snd) (NE.toList legalEval)
  return $
    Options
      (buildSafeNonempty (fmap fst stillLegal) def)
      (M.fromList newlyIllegal <> illegal)
      p

-- THIS IS UNSAFE!
unlessYouCould' :: (Eq i, Ord pl) => (pl -> pl -> Legality i) -> Options pl i -> Options pl i
unlessYouCould' comparer (Options legal illegal p) =
  let legal' = NE.toList legal
      new = [(play, mconcat (fmap (comparer play) legal')) | play <- legal']
      (newLegalMap, newIllegal) = M.partition (== Legal) (M.fromList new)
   in Options (NE.fromList (M.keys newLegalMap)) (newIllegal <> illegal) p

unlessYouCould :: (Ord pl, Eq i, Monad m) => (pl -> pl -> m (Legality i)) -> m (Options pl i) -> m (Options pl i)
unlessYouCould mcomparer mopts = do
  (Options legal illegal p) <- mopts
  let legal' = NE.toList legal
  new' <- traverse sequence $ [(play, traverse (mcomparer play) legal') | play <- legal']
  let new = fmap (fmap mconcat) new'
  let (newLegalMap, newIllegal) = M.partition (== Legal) (M.fromList new)
  return (Options (NE.fromList (M.keys newLegalMap)) (newIllegal <> illegal) p)
