{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Util where
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Bifunctor (Bifunctor(..))
import Control.Applicative (Applicative(..), liftA2)
import Data.List (sortOn)
import qualified Data.Foldable as F
import Effectful ((:>), Eff)
import Effectful.Crypto.RNG (RNG, randomR)
import System.Random.Shuffle (shuffle)

graph :: (t -> b) -> t -> (t, b)
graph f a = (a, f a)

graphM :: Functor m => (a -> m b) -> (a -> m (a,b))
graphM f a = fmap (a,) (f a)


graphMap :: Ord a => (a -> Maybe b) -> [a] -> Map a b
graphMap fab = M.fromList . mapMaybe (graphM fab)

graphMapM :: (Monad m, Ord a) => (a -> m (Maybe b)) -> [a] -> m (Map a b)
graphMapM fab as = do
    theGraph <- traverse (graphM fab) as
    return . M.fromList . mapMaybe sequence $ theGraph

getNext :: Eq a => a -> NE.NonEmpty a -> Maybe a
getNext x (y :| (y':ys)) = if x == y
                      then Just y'
                      else getNext x (y' :| ys)
getNext _ (_ :| []) = Nothing

buildSafeNonempty :: [a] -> a -> NonEmpty a
buildSafeNonempty xs def = if null xs then def :| [] else NE.fromList xs

cartesianProduct :: [a] -> [b] -> [(a,b)]
cartesianProduct as bs = [(a,b) | a <- as, b <- bs]

splitOnFirst :: (a -> Bool) -> [a] -> ([a],[a])
splitOnFirst pred  (a:as) = if pred a
                            then ([a], as)
                            else first (a:) $ splitOnFirst pred as
splitOnFirst _ [] = ([],[])

getNextCyclic :: Eq a => a -> NE.NonEmpty a -> Maybe a
getNextCyclic x ys = case getNext x ys of
                       Just y' -> Just y'
                       Nothing -> if x == NE.last ys
                                  then Just (NE.head ys)
                                  else Nothing


maximaBy :: (a -> a -> Ordering) -> [a] -> [a]
maximaBy cmp as = go cmp as [] where
    go cmp (a:remaining) [] = go cmp remaining [a]
    go cmp (a:remaining) (m:maxes) = case cmp a m of
                                       LT -> go cmp remaining (m:maxes)
                                       EQ -> go cmp remaining (a:m:maxes)
                                       GT -> go cmp remaining [a]
    go _ [] maxes = maxes



maximaByScore :: forall a m . Monad m => (a -> m Int) -> [a] -> m [a]
maximaByScore score as = fmap fst . takeFirstsOn snd . sortOn snd <$> traverse (graphM score) as

takeFirstsOn :: Eq b => (a -> b) -> [a] -> [a]
takeFirstsOn f as = go f as [] where
    go f (x:ys) [] = go f ys [x]
    go f (x:ys) (first:firsts) = if f x == f first then go f ys (x:first:firsts)
                                                   else first:firsts
    go _ [] firsts = firsts

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mbool mtrue mfalse = do
    boolResult <- mbool
    if boolResult
    then mtrue
    else mfalse

andA :: Applicative m => m Bool -> m Bool -> m Bool
andA = liftA2 (&&)

safeIndexList :: Foldable f => Int -> f a -> Maybe a
safeIndexList i xs = if i < 0 then Nothing else safeIndexList' i (F.toList xs)
                         where
                             safeIndexList' i (x:xs) = if i == 0 then Just x else safeIndexList' (i-1) xs
                             safeIndexList' _ [] = Nothing

shuffleRNG :: RNG :> es => [a] -> Eff es [a]
shuffleRNG elements
    | null elements = return []
    | otherwise = fmap (shuffle elements) (rseqM (length elements - 1))
    where
        rseqM :: RNG :> es => Int -> Eff es [Int]
        rseqM 0 = return []
        rseqM i = liftA2 (:) (randomR (0,i)) (rseqM (i-1))
