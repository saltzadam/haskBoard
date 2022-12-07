module Util where

import Data.Map (Map)
import qualified Data.Map as M

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

root :: Enum a => a
root = toEnum 0

enumerateFromRoot :: (Bounded a, Enum a) => [a]
enumerateFromRoot = toEnum <$> [0 .. maxBound]

enumConstMap :: (Enum e, Bounded e, Ord e) => a -> Map e a
enumConstMap y = M.fromList [(x, y) | x <- enumerateFromRoot]
