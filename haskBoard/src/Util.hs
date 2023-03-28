{-# LANGUAGE TupleSections #-}
module Util where
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (mapMaybe)

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


