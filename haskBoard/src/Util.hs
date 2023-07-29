{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Util where

import Control.Applicative (Applicative (..), liftA2)
import Control.Monad ((>=>))
import Data.Bifunctor (Bifunctor (..))
import Data.Finitary (Finitary, inhabitants)
import qualified Data.Foldable as F
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isJust, mapMaybe)
import Data.Monoid (Endo (..))
import Data.Semigroup (mtimesDefault)
import Data.Semigroup (stimes)
import qualified Data.Set as S
import Data.Tuple (swap)
import Effectful (Eff, (:>))
import Effectful.Crypto.RNG (RNG, randomR)
import System.Random.Shuffle (shuffle)

graph :: (t -> b) -> t -> (t, b)
graph f a = (a, f a)

graphM :: Functor m => (a -> m b) -> (a -> m (a, b))
graphM f a = fmap (a,) (f a)

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

kleisliCompose :: (Monad m) => [a -> m a] -> (a -> m a)
kleisliCompose = foldr (>=>) return

graphMap :: Ord a => (a -> Maybe b) -> [a] -> Map a b
graphMap fab = M.fromList . mapMaybe (graphM fab)

graphMapM :: (Monad m, Ord a) => (a -> m (Maybe b)) -> [a] -> m (Map a b)
graphMapM fab as = do
  theGraph <- traverse (graphM fab) as
  return . M.fromList . mapMaybe sequence $ theGraph

getNext :: Eq a => a -> NE.NonEmpty a -> Maybe a
getNext x (y :| (y' : ys)) =
  if x == y
    then Just y'
    else getNext x (y' :| ys)
getNext _ (_ :| []) = Nothing

buildSafeNonempty :: [a] -> a -> NonEmpty a
buildSafeNonempty xs def = if null xs then def :| [] else NE.fromList xs

ifNullElse :: [a] -> NonEmpty a -> NonEmpty a
ifNullElse tested def = fromMaybe def (NE.nonEmpty tested)

mapFromFun :: Ord a => (a -> b) -> [a] -> Map a b
mapFromFun f xs = M.fromSet f (S.fromList xs)

inhabitantsSet :: (Finitary a, Ord a) => S.Set a
inhabitantsSet = S.fromList inhabitants

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct as bs = [(a, b) | a <- as, b <- bs]

splitOnFirst :: (a -> Bool) -> [a] -> ([a], [a])
splitOnFirst pred (a : as) =
  if pred a
    then ([a], as)
    else first (a :) $ splitOnFirst pred as
splitOnFirst _ [] = ([], [])

getNextCyclic :: Eq a => a -> NE.NonEmpty a -> Maybe a
getNextCyclic x ys = case getNext x ys of
  Just y' -> Just y'
  Nothing ->
    if x == NE.last ys
      then Just (NE.head ys)
      else Nothing

maximaBy :: (a -> a -> Ordering) -> [a] -> [a]
maximaBy cmp as = go cmp as []
  where
    go cmp (a : remaining) [] = go cmp remaining [a]
    go cmp (a : remaining) (m : maxes) = case cmp a m of
      LT -> go cmp remaining (m : maxes)
      EQ -> go cmp remaining (a : m : maxes)
      GT -> go cmp remaining [a]
    go _ [] maxes = maxes

maximaByScore :: forall a m. Monad m => (a -> m Int) -> [a] -> m [a]
maximaByScore score as = fmap fst . takeFirstsOn snd . sortOn snd <$> traverse (graphM score) as

takeFirstsOn :: Eq b => (a -> b) -> [a] -> [a]
takeFirstsOn f as = go f as []
  where
    go f (x : ys) [] = go f ys [x]
    go f (x : ys) (first : firsts) =
      if f x == f first
        then go f ys (x : first : firsts)
        else first : firsts
    go _ [] firsts = firsts

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mbool mtrue mfalse = do
  boolResult <- mbool
  if boolResult
    then mtrue
    else mfalse

pureIfM :: Monad m => m Bool -> a -> a -> m a
pureIfM mbool true false = ifM mbool (pure true) (pure false)

andA :: Applicative m => m Bool -> m Bool -> m Bool
andA = liftA2 (&&)

safeIndexList :: Foldable f => Int -> f a -> Maybe a
safeIndexList i xs = if i < 0 then Nothing else safeIndexList' i (F.toList xs)
  where
    safeIndexList' i (x : xs) = if i == 0 then Just x else safeIndexList' (i - 1) xs
    safeIndexList' _ [] = Nothing

shuffleRNG :: RNG :> es => [a] -> Eff es [a]
shuffleRNG elements
  | null elements = return []
  | otherwise = fmap (shuffle elements) (rseqM (length elements - 1))
  where
    rseqM :: RNG :> es => Int -> Eff es [Int]
    rseqM 0 = return []
    rseqM i = liftA2 (:) (randomR (0, i)) (rseqM (i - 1))

-- TODO: inefficient, see M.mapMaybeWithKey
mapKeysMaybeWith :: Ord k' => (v -> v -> v) -> (k -> Maybe k') -> Map k v -> Map k' v
mapKeysMaybeWith aggv f = M.mapKeys fromJust . M.filterWithKey (\maybeK _ -> isJust maybeK) . M.mapKeysWith aggv f

mapKeysMaybe :: (Ord k', Ord v) => (k -> Maybe k') -> Map k v -> Map k' v
mapKeysMaybe = mapKeysMaybeWith min

groupBy :: (Semigroup v, Ord k') => (k -> k') -> Map k v -> Map k' v
groupBy = M.mapKeysWith (<>)

groupByMaybe :: (Ord k', Semigroup v) => (k -> Maybe k') -> Map k v -> Map k' v
groupByMaybe = mapKeysMaybeWith (<>)

uncurryMap :: Ord k => Map (k, k') v -> Map k (Map k' v)
uncurryMap = M.mapKeys fst . M.mapWithKey (\(_, k') v -> M.singleton k' v)

curryMap :: (Ord k, Ord k') => Map k (Map k' v) -> Map (k, k') v
curryMap = M.unions . fmap snd . M.toList . M.mapWithKey (\k mapk' -> M.mapKeys (k,) mapk')

invertNestedMaps :: (Ord k, Ord k') => Map k (Map k' v) -> Map k' (Map k v)
invertNestedMaps = uncurryMap . M.mapKeys swap . curryMap

foldMapNE :: Semigroup t1 => (t2 -> t1) -> NonEmpty t2 -> t1
foldMapNE f (x :| xs) = go (f x) xs
  where
    go y [] = y
    go y (z : zs) = y <> go (f z) zs

foldrNE :: (t2 -> a -> a) -> a -> NonEmpty t2 -> a
foldrNE f z xs = appEndo (foldMapNE (Endo . f) xs) z

concatNE :: NonEmpty (NonEmpty a) -> NonEmpty a
concatNE = foldMapNE id

-- repeatUntilStable :: Eq a => (a -> a) -> a -> a
-- repeatUntilStable f x = fst . head $ dropWhile (uncurry (/=)) (zip (iterate f (f x)) (iterate f x))

-- posetMaxima :: Foldable f => (a -> a -> Bool) -> f a -> [a]
-- posetMaxima comp fs = undefined comp fs [] where
--     go :: (a -> a -> Bool) -> (f a, [a]) -> [a]
--     go comp' fs' maxes = F.foldl' (\(maxes', newElem) -> case partition (comp newElem) maxes' of

--     )
mapMaybeMap :: Ord a => (a -> Maybe b) -> [a] -> Map a b
mapMaybeMap f = M.fromList . mapMaybe (sequence . graph f)

mapMaybeMapM :: (Ord a, Applicative f) => (a -> f (Maybe b)) -> [a] -> f (Map a b)
mapMaybeMapM fm xs = fmap (M.fromList . mapMaybe sequence) (traverse (graphM fm) xs)

mapKeysCatMaybes :: Ord k' => (k -> Maybe k') -> Map k a -> Map k' a
mapKeysCatMaybes fm = M.mapKeys fromJust . M.filterWithKey (\k _ -> isJust k) . M.mapKeys fm

boolToInt :: Num a => Bool -> a
boolToInt True = 1
boolToInt False = 0

catRights :: [Either a b] -> [b]
catRights = mconcat . fmap F.toList

mkPairs :: [a] -> [[(a, a)]]
mkPairs [a, b] = [[(a, b)]]
mkPairs (x : xs) = concat [((x, selection) :) <$> mkPairs remainder | (selection, remainder) <- allButs xs]
  where
    allButs :: [a] -> [(a, [a])]
    allButs xs = fmap (\(a, b, c) -> (a, reverse b ++ c)) (go xs [])
      where
        go :: [a] -> [a] -> [(a, [a], [a])]
        go (y : ys) revHeads = (y, revHeads, ys) : go ys (y : revHeads)
        go [] _ = []
mkPairs [] = []

fromEither :: c -> c -> Either a b -> c
fromEither x _ (Left _) = x
fromEither _ y (Right _) = y

mtimes :: Monoid m => Int -> m -> m
mtimes = mtimesDefault
