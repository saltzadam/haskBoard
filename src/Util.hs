module Util where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Finitary
import Control.Lens (lens)

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

root :: Enum a => a
root = toEnum 0

enumerateFromRoot :: (Bounded a, Enum a) => [a]
enumerateFromRoot = toEnum <$> [0 .. maxBound]

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

