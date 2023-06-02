{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Util where
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Bifunctor (Bifunctor(..))
import Control.Applicative (Applicative(..))

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
maximaByScore score as = go score as [] where
    go :: (a -> m Int) -> [a] -> [a] -> m [a]
    go score (a:remaining) [] = go score remaining [a]
    go _ [] maxes = return maxes
    go score (a:remaining) (m:maxes) = do
        scorea <- score a
        scorem <- score m
        case compare scorea scorem of
          LT -> go score remaining (m:maxes)
          EQ -> go score remaining (a:m:maxes)
          GT -> go score remaining [a]
        
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mbool mtrue mfalse = do
    boolResult <- mbool
    if boolResult
    then mtrue
    else mfalse

andA :: Applicative m => m Bool -> m Bool -> m Bool
andA = liftA2 (&&)
