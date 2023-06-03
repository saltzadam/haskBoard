{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module FinitaryMap 
    (FTMap (..)
    , (!!!)
    , applyAt
    , ftAt)
    where
import Prelude
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)
import GHC.Base (liftA2)
import Data.Finitary (Finitary, inhabitants)
import Control.Lens (lens)

-- FTMap is a "Finitary, total map". That is, it's a total map from a finitary domain. 
-- It's represented as a newtype wrapper on `a -> b` where a is Finitary.

newtype FTMap a b = FTMap {runFn :: a -> b} deriving (Generic, Functor, Applicative)

-- From function to Map using the finitary-ness of `a`. 
reifyFn :: (Finitary a, Eq a) => FTMap a b -> Map a b
reifyFn (FTMap f) = M.fromAscList [(a, f a) | a <- inhabitants]

-- unsafe because the map isn't required to be total. 
unsafeUnreify :: (Ord a, Finitary a) => Map a b -> FTMap a b
unsafeUnreify m = FTMap (m M.!)

instance (Eq a, Eq b, Finitary a) => Eq (FTMap a b) where
    (==) f g = reifyFn f == reifyFn g

instance (Show a, Show b, Finitary a, Eq a) => Show (FTMap a b) where
    show f = show (reifyFn f)

instance (Eq a, Finitary a) => Foldable (FTMap a) where
    foldMap g = foldMap g . reifyFn

instance Semigroup b => Semigroup (FTMap a b) where
    (<>) = liftA2 (<>)

instance Monoid b => Monoid (FTMap a b) where
    mempty = FTMap (const mempty)

instance Num b => Num (FTMap a b) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger i = FTMap (const (fromInteger i))

instance (Finitary a, Ord a, Ord b) => Ord (FTMap a b) where
    compare f g = compare (reifyFn f) (reifyFn g)

-- TODO: change to !!! and import qualified
(!!!) :: FTMap a b -> a -> b
(!!!) (FTMap f) = f

applyAt :: Eq a => a -> (b -> b) -> FTMap a b -> FTMap a b
applyAt a fn f = FTMap (\x -> if x == a then fn (f !!! x) else f !!! x)

update :: Eq a => (a,b) -> FTMap a b  -> FTMap a b
update (a,b) f = FTMap (\x -> if x == a then b else f !!! x)

-- TODO : wonder if this could be used elsewhere
filter :: (Finitary a, Eq a) => (b -> Bool) -> FTMap a b -> Map a b
filter filt = M.filter filt .  reifyFn

-- Lenses

ftAt :: (Eq a, Functor f) => a -> (b -> f b) -> FTMap a b -> f (FTMap a b)
ftAt x = lens (!!! x) (\f y -> update (x,y) f)


