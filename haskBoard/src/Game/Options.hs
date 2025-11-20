{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Options where

import Control.Applicative (liftA3)
import Control.Lens (makeFields)
import Control.Monad (filterM)
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

-- -- A move may have lots of illegality.
-- -- Concatenate where possible.
--
-- data Legality illegal = Legal | Illegal (NE.NonEmpty illegal) deriving (Eq, Ord, Show, Generic, Functor)
--
-- oneIssue :: illegal -> Legality illegal
-- oneIssue i = Illegal (NE.singleton i)
--
-- isIllegal :: Legality illegal -> Bool
-- isIllegal (Illegal _) = True
-- isIllegal _ = False
--
-- instance Semigroup (Legality i) where
--   Legal <> x = x
--   x <> Legal = x
--   Illegal x <> Illegal y = Illegal (x <> y)
--
-- instance Monoid (Legality i) where
--   mempty = Legal
--
-- firstLegal :: (Eq i) => (pl -> Legality i) -> [pl] -> Maybe pl
-- firstLegal check (play : plays) =
--   if check play == Legal
--     then Just play
--     else firstLegal check plays
-- firstLegal _ [] = Nothing
--
-- firstLegalM :: (Eq illegal, Monad m) => (b -> m (Legality illegal)) -> [b] -> m (Maybe b)
-- firstLegalM check (play : plays) = do
--   checkM <- check play
--   if checkM == Legal
--     then return $ Just play
--     else firstLegalM check plays
-- firstLegalM _ [] = return Nothing
--
-- data Options pl = Options
data Options pl = Options
  { legal :: NonEmpty pl,
    -- illegal :: Map pl (Legality i),
    owner :: Player
  }
  deriving (Eq, Ord, Show, Generic)

makeFields ''Options

-- displayOptions :: (Show pl, Show i) => Options pl -> String
-- displayOptions (Options legalO illegalO _) =
--   show (NE.toList legalO)
--     ++ "\n"
--     ++ "Cannot choose: "
--     ++ showMapAsList illegalO
--   where
--     showMapAsList :: (Show pl, Show i) => Map pl (Legality i) -> String
--     showMapAsList = show . M.toList
--
-- opts0 <> opts1 is opts0 overridden w/ opts1

instance Semigroup (Options pl) where
  Options l o <> Options l' _ = Options (l <> l') o

-- instance (Ord pl, Eq i) => Semigroup (Options pl) where
--   (Options legal illegal owner) <> (Options legal' illegal' _) =
--     let asMap = M.fromList [(play, Legal) | play <- NE.toList legal] <> illegal
--         asMap' = M.fromList [(play, Legal) | play <- NE.toList legal'] <> illegal'
--         newMap = asMap <> asMap'
--         (newLegal, newIllegal) = M.partition (== Legal) newMap
--      in Options (NE.fromList (M.keys newLegal)) newIllegal owner -- unsafe NE.fromList but newLegal is nonempty
--
-- raiseIssueIf :: issue -> Bool -> Legality issue
-- raiseIssueIf = flip mustNotElse
--
-- mustElse :: Bool -> issue -> Legality issue
-- mustElse True _ = Legal
-- mustElse False i = Illegal [i]
--
-- mustNotElse :: Bool -> issue -> Legality issue
-- mustNotElse True i = Illegal [i]
-- mustNotElse False _ = Legal
--

-- -- None of these three really capture how games are written
-- buildOptions' :: (Eq issue, Ord play, Foldable t) => Player -> play -> t (play, Legality issue) -> Options play issue
-- buildOptions' p defaultPlay playMap =
--   let (legalMoves, illegalMoves) = M.partition (== Legal) . M.fromList . F.toList $ playMap
--    in Options
--         (buildSafeNonempty (M.keys legalMoves) defaultPlay)
--         illegalMoves
--         p
--
-- buildOptions'' :: (Eq i) => Player -> pl -> Map pl (Legality i) -> Options pl
-- buildOptions'' p defaultPlay playMap =
--   let (legalMoves, illegalMoves) = M.partition (== Legal) playMap
--    in Options
--         (buildSafeNonempty (M.keys legalMoves) defaultPlay)
--         illegalMoves
--         p
--
-- buildOptions :: (Monad m, Traversable t, Eq issue, Ord play, Foldable m) => Player -> (play -> m (Legality issue)) -> play -> m (t play) -> m (Options play issue)
-- buildOptions p checkPlay defaultPlay plays =
--   let playsSet = S.fromList . F.toList <$> plays
--       playsMap = (sequence . M.fromSet checkPlay) =<< playsSet
--    in buildOptions'' p defaultPlay <$> playsMap
--
-- This looks better
youMay' :: Player -> NonEmpty pl -> Options pl
youMay' p basePlays = Options basePlays p

youMay :: (Functor f) => Player -> f (NonEmpty pl) -> f (Options pl)
youMay p mBasePlays = youMay' p <$> mBasePlays

youMayNot' :: [pl] -> Options pl -> Options pl
youMayNot' pls (Options opls p) = Options (NE.appendList opls pls) p

youMayNot :: (Functor f) => [pl] -> f (Options pl) -> f (Options pl)
youMayNot pls mBasePlays = youMayNot' pls <$> mBasePlays

-- filter out plays IF THE CONDITION IS TRUE
exceptIf :: (Monad m) => (pl -> m Bool) -> m pl -> m (Options pl) -> m (Options pl)
exceptIf mfilt mdefault mopts = do
  let exceptFilt m = not <$> mfilt m
  def <- mdefault
  Options plays player <- mopts
  filteredPlays <- filterM exceptFilt (NE.toList plays)
  return (Options (buildSafeNonempty filteredPlays def) player)

-- makeIllegal' :: (Ord pl) => pl -> Legality i -> pl -> Options pl -> Options pl
-- makeIllegal' play issue def (Options legal illegal p) =
--   Options
--     (buildSafeNonempty (delete play (NE.toList legal)) def)
--     (M.insert play issue illegal)
--     p
--
-- exceptIfMap' :: Ord pl => Map pl (Legality i) -> pl -> Options pl -> Options pl
-- exceptIfMap' issueMap def opts =
--   let issueTuples = M.toList issueMap
--       makeAllIllegal = compose (($ def) . uncurry makeIllegal' <$> issueTuples)
--    in makeAllIllegal opts

-- exceptIfMap :: (Applicative m, Ord pl) => m (Map pl (Legality i)) -> m pl -> m (Options pl) -> m (Options pl)
-- exceptIfMap = liftA3 exceptIfMap'
--
-- exceptIf' :: (Ord pl, Eq i) => (pl -> Legality i) -> pl -> Options pl -> Options pl
-- exceptIf' issuef def (Options legal illegal p) =
--   let (stillLegal, newlyIllegal) = partition ((== Legal) . issuef) (NE.toList legal)
--    in Options
--         (buildSafeNonempty stillLegal def)
--         (M.fromSet issuef (S.fromList newlyIllegal) <> illegal)
--         p
--
-- exceptIf :: (Ord pl, Eq i, Monad m) => (pl -> m (Legality i)) -> m pl -> m (Options pl) -> m (Options pl)
-- exceptIf issuefm mdefault mopts = do
--   def <- mdefault
--   Options legal illegal p <- mopts
--   legalEval <- traverse (graphM issuefm) legal
--   let (stillLegal, newlyIllegal) = partition ((== Legal) . snd) (NE.toList legalEval)
--   return $
--     Options
--       (buildSafeNonempty (fmap fst stillLegal) def)
--       (M.fromList newlyIllegal <> illegal)
--       p

-- THIS IS UNSAFE!
-- The idea is "if playA is legal then playB should not be." But you could also have "if playB is legal then playA should not be" encoded in the same `comparer`. I.e. need the comparer to be transitive.
-- Could be solved by a PartialOrd instance on pl, but it might be situational. (Also PartialOrd can't check for transitivity)
-- I don't think it's possible to write a safe function
--   neDeleteIfContains toDelete ifContains as = if ifContains `elem` as
--                                               then deleteSafely toDelete as
--                                               else as
-- because neDeleteIfContains a a would just be delete.

-- comparer (ifContains) (maybeIllegal)

help :: (pl -> pl -> Bool) -> [pl] -> [pl]
help comparer list =
  let filters x = all (($ x) . comparer) list
   in filter filters list

helpM :: (Applicative m) => (pl -> pl -> m Bool) -> [pl] -> m [pl]
helpM mcomparer list = do
  let filters x = and <$> traverse (($ x) . mcomparer) list
  filterM filters list

-- mcomparer a b = if a is in mopts then remove b
unlessYouCould :: (Ord pl, Monad m) => (pl -> pl -> m Bool) -> m (Options pl) -> m (Options pl)
unlessYouCould mcomparer mopts = do
  Options legal p <- mopts
  newLegal <- helpM mcomparer (NE.toList legal)
  return $ Options (NE.fromList newLegal) p

-- unlessYouCould' :: (Eq i, Ord pl) => (pl -> pl -> Legality i) -> Options pl -> Options pl
-- unlessYouCould' comparer (Options legal illegal p) =
--   let legal' = NE.toList legal
--       new = [(play, mconcat (fmap (comparer play) legal')) | play <- legal']
--       (newLegalMap, newIllegal) = M.partition (== Legal) (M.fromList new)
--    in Options (NE.fromList (M.keys newLegalMap)) (newIllegal <> illegal) p
--
-- -- SO IS THIS
-- unlessYouCould :: (Ord pl, Eq i, Monad m) => (pl -> pl -> m (Legality i)) -> m (Options pl) -> m (Options pl)
-- unlessYouCould mcomparer mopts = do
--   (Options legal illegal p) <- mopts
--   let legal' = NE.toList legal
--   new' <- traverse sequence $ [(play, traverse (mcomparer play) legal') | play <- legal']
--   let new = fmap (fmap mconcat) new'
--   let (newLegalMap, newIllegal) = M.partition (== Legal) (M.fromList new)
--   return (Options (NE.fromList (M.keys newLegalMap)) (newIllegal <> illegal) p)
