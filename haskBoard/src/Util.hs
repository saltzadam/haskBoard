module Util where
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Map (Map)
import qualified Data.Map as M
import Data.Finitary
import Control.Lens (lens)
import Data.List (findIndex, elemIndex)
import Data.Maybe (listToMaybe)

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

root :: Enum a => a
root = toEnum 0

enumerateFromRoot :: (Bounded a, Enum a) => [a]
enumerateFromRoot = toEnum <$> [0 .. maxBound]

coerceEnum :: (Enum a, Enum b) => a -> b
coerceEnum = toEnum . fromEnum

unsafeNextCyclic :: Eq a => a -> [a] -> a
unsafeNextCyclic y xs = go y xs xs where
    go :: Eq a => a -> [a] -> [a] -> a
    go y (x:x':xs) xs' = if y == x then x' else go y (x':xs) xs'
    go y [_] xs' = head xs'
    go y [] xs' = error "unsafeNextCyclic x xs wit hx not in xs"

enumConstMap :: (Enum e, Bounded e, Ord e) => a -> Map e a
enumConstMap y = M.fromList [(x, y) | x <- enumerateFromRoot]

maybeToBool :: Maybe Bool -> Bool
maybeToBool (Just True) = True
maybeToBool _ = False

updatef :: Eq a => (a,b) -> (a -> b)-> (a -> b)
updatef (x0,y) f x1 = if x0 == x1 then y else f x1

mapFinitary :: (Finitary a, Ord a) => (a -> b) -> Map a b
mapFinitary f = M.fromAscList [(a, f a) | a <- inhabitants]

-- given f :: a -> b
fnLens :: (Functor f, Eq t) => ((t -> p) -> f t) -> (t -> p) -> f (p -> t -> p)
fnLens = lens id (\f a b x -> if x == a then b else f x)

reifyFn :: (Finitary a, Ord a) => (a -> b) -> Map a b
reifyFn f = M.fromAscList [(a, f a) | a <- inhabitants]
