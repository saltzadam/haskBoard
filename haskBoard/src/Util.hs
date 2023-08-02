{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Util where

import Control.Monad ((>=>))
import Data.Finitary (Finitary, inhabitants)
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isJust, mapMaybe)
import Data.Semigroup.Foldable (foldMap1)
import qualified Data.Set as S
import Data.Tuple (swap)

-- TODO: assess whether these two are ever actually necessary
graph :: (t -> b) -> t -> (t, b)
graph f a = (a, f a)

graphM :: Functor m => (a -> m b) -> (a -> m (a, b))
graphM f a = fmap (a,) (f a)

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

kleisliCompose :: (Monad m) => [a -> m a] -> (a -> m a)
kleisliCompose = foldr (>=>) return

getNext :: Eq a => a -> NE.NonEmpty a -> Maybe a
-- look for first match, then take next item. Zip together list and tail so we can fold
-- will always be Nothing on empty or one-item lists
getNext match items = foldr (\(x, x') acc -> if x == match then Just x' else acc) Nothing (zip (NE.toList items) (NE.tail items))

getNextCyclic :: Eq a => a -> NE.NonEmpty a -> Maybe a
getNextCyclic match items =
  foldr
    (\(x, x') acc -> if x == match then Just x' else acc)
    Nothing
    ( zip
        (NE.toList items)
        (NE.toList . shift $ items)
    )
  where
    shift (a :| as) = NE.appendList (NE.singleton a) as

-- TODO: these two should be related
ifNullElse :: [a] -> NonEmpty a -> NonEmpty a
ifNullElse tested def = fromMaybe def (NE.nonEmpty tested)

buildSafeNonempty :: [a] -> a -> NonEmpty a
buildSafeNonempty xs def = if null xs then NE.singleton def else NE.fromList xs

inhabitantsSet :: (Finitary a, Ord a) => S.Set a
inhabitantsSet = S.fromList inhabitants

maximaBy :: forall a. (a -> a -> Ordering) -> [a] -> [a]
maximaBy cmp = foldr (go cmp) []
  where
    go :: (a -> a -> Ordering) -> a -> [a] -> [a]
    go _ a [] = [a]
    go cmp a currMaxes@(m : _) = case cmp a m of
      LT -> currMaxes
      EQ -> a : currMaxes
      GT -> [a]

{- now go cmp a = fun (cmp a m)
-- fun :: Ordering -> a -> [a] -> [a]
-- fun LT _ = id
-- fun EQ a = (a :)
-- fun GT a = const [a]
-}

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mbool mtrue mfalse = do
  boolResult <- mbool
  if boolResult
    then mtrue
    else mfalse

pureIfM :: Monad m => m Bool -> a -> a -> m a
pureIfM mbool true false = ifM mbool (pure true) (pure false)

safeIndexList :: Foldable f => Int -> f a -> Maybe a
safeIndexList i xs = if i < 0 then Nothing else safeIndexList' i (F.toList xs)
  where
    safeIndexList' i (x : xs) = if i == 0 then Just x else safeIndexList' (i - 1) xs
    safeIndexList' _ [] = Nothing

invertNestedMaps :: (Ord k, Ord k') => Map k (Map k' v) -> Map k' (Map k v)
invertNestedMaps = uncurryMap . M.mapKeys swap . curryMap
  where
    uncurryMap :: Ord k => Map (k, k') v -> Map k (Map k' v)
    uncurryMap = M.mapKeys fst . M.mapWithKey (\(_, k') v -> M.singleton k' v)

    curryMap :: (Ord k, Ord k') => Map k (Map k' v) -> Map (k, k') v
    curryMap = M.unions . fmap snd . M.toList . M.mapWithKey (\k mapk' -> M.mapKeys (k,) mapk')

concatNE :: NonEmpty (NonEmpty a) -> NonEmpty a
concatNE = foldMap1 id

mapMaybeMap :: Ord a => (a -> Maybe b) -> [a] -> Map a b
mapMaybeMap f = M.fromList . mapMaybe (sequence . graph f)

mapMaybeMapM :: (Ord a, Applicative f) => (a -> f (Maybe b)) -> [a] -> f (Map a b)
mapMaybeMapM fm xs = fmap (M.fromList . mapMaybe sequence) (traverse (graphM fm) xs)

mapKeysCatMaybes :: Ord k' => (k -> Maybe k') -> Map k a -> Map k' a
mapKeysCatMaybes fm = M.mapKeys fromJust . M.filterWithKey (\k _ -> isJust k) . M.mapKeys fm

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
