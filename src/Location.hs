{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DerivingVia #-}
module Location where
import Data.Map (Map)
import qualified Data.Map as M
import Count
import Data.Bifunctor (first)
import Data.Finitary (Finitary (..), inhabitants)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Monoid (First(..))
import GHC.Base (Applicative(..))

-- Try again with associated types
-- just like https://wiki.haskell.org/GHC/Type_families
-- except `insert` isn't public

class Location f slots where
    empty :: f slots a
    moveTo :: a -> f slots a -> (f slots a, Maybe a)
    moveFrom :: Eq a => a -> f slots a -> (f slots a, Maybe a)
    histogram :: Ord a => f slots a -> Map a (Cnt Int)
    lookup :: f slots a -> slots -> Maybe a

instance Location Map Int where
    empty = M.empty
    moveTo a stack = (M.insert ((+1) . fst . M.findMax $ stack) a stack, Just a)
    moveFrom a stack = case listToMaybe . M.toList $ M.filter (== a) stack of
                        Nothing -> (stack, Nothing)
                        Just (i, _) -> (downshift i . M.delete i $ stack, Just a) where
                            downshift i = M.mapKeys (\j -> if j > i then j - 1 else j)
    histogram = histogramF
    lookup = flip M.lookup

newtype Stack' i a = Stack' {getStack :: Map i a} deriving (Eq, Ord, Show, Foldable, Functor)

instance (Ord i, Finitary i) => Location Stack' i where
    empty = Stack' empty
    moveTo a = first Stack' . moveTo a . getStack
    moveFrom a = first Stack' . moveFrom a . getStack
    histogram = histogramF
    lookup = flip M.lookup . getStack

newtype Cards = Card Int deriving (Eq, Show, Ord)
type Deck = Map Int


pile :: Int -> a -> Deck a
pile i a = M.fromList (zip [0..] (replicate i a))

single :: a -> Deck a
single = pile 1

-- assumes `es` is ascending!!
findFirstMissing :: (Finitary e) => [e] -> Maybe e
findFirstMissing es = go es inhabitants where
    go :: Eq e => [e] -> [e] -> Maybe e
    go (e:es) (i:inh) | e == i = go es inh
                      | otherwise = Just e
    go (e:_) [] = Just e
    go _ _ = Nothing

findFirstEmptySlot :: (Finitary e) => Map e a -> Maybe e
findFirstEmptySlot = findFirstMissing . M.keys

instance (Ord enum, Finitary enum) => Location Map enum where
    empty = M.empty
    moveTo a aMap = case findFirstEmptySlot aMap of
                       Nothing -> (aMap, Nothing)
                       Just e -> (M.insert e a aMap, Just a)
    moveFrom a aMap = case listToMaybe . M.toList . M.filter (== a) $ aMap of
                         Nothing -> (aMap, Nothing)
                         Just (e, _) -> (M.delete e aMap, Just a)
    histogram = histogramF
    lookup = flip M.lookup


newtype InfStackC s a = InfStackC {runInf :: Maybe a} deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Location InfStackC () where
    empty = InfStackC Nothing
    moveTo _ stack = (stack, Nothing)
    moveFrom b stack = if Just b == runInf stack then (stack, Just b) else (stack, Nothing)
    histogram stack = case runInf stack of
                         Just a -> M.singleton a Infinity
                         Nothing -> M.empty
    lookup stack = const (runInf stack)




firstEmptyKey :: IntMap a -> Maybe Int
firstEmptyKey im = getFirst $ foldMap (\(asc, imkey) -> First (if imkey > asc then Just asc else Nothing)) (zip [1..] (IM.keys im))

moveToStack :: a -> IntMap a -> (IntMap a, Maybe a)
moveToStack a im = case firstEmptyKey im of
                     Just i -> (IM.insert i a im, Just a)
                     Nothing -> (im, Nothing)

findInIntMap :: Eq a => IntMap a -> a -> Maybe Int
findInIntMap im a = getFirst . foldMap (\(i,b) -> First (if a == b then Just i else Nothing)) . IM.toList $ im

moveFromStack :: Eq a => a -> IntMap a -> (IntMap a, Maybe a)
moveFromStack a im = case findInIntMap im a of
                       Just i -> (IM.delete i im, Just a)
                       Nothing -> (im, Nothing)

-- check if loc0 covers loc1 given some wildcards
deficitWithWildcards :: (Ord a, Location f s, Location f' s') => f s a -> f' s' a -> [a] -> Cnt Int
deficitWithWildcards loc0 loc1 wildcards = deficit - nWildcards
    where
        nWildcards = sum (M.filterWithKey (\k _ -> k `elem` wildcards) hist0)
        maybeDeficit = fmap sum . sequence $ [liftA2 subtract (M.lookup k hist1) (M.lookup k hist0) | k <- M.keys hist0]
        deficit = fromMaybe Infinity maybeDeficit
        hist0 = histogram loc0
        hist1 = histogram loc1


deficit :: (Ord a, Location f s, Location f' s') => f s a -> f' s' a -> Cnt Int
deficit loc0 loc1 = deficitWithWildcards loc0 loc1 []
