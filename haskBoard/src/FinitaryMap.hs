{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
module FinitaryMap where
import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)
import GHC.Base (liftA2)
import Control.Lens ( lens )
import Control.Lens.Iso

-- FTMap is a "Finitary, total map". That is, it's a total map from a finitary domain. 
-- It's represented as a newtype wrapper on `a -> b` where a is Finitary.

newtype FTMap a b = FTMap {runFn :: a -> b} deriving (Generic, Functor, Applicative)

type FakeFinitary a = (Enum a, Bounded a)

inhabitants :: FakeFinitary a => [a]
inhabitants = [minBound ..maxBound]

-- From function to Map using the finitary-ness of `a`. 
-- TODO: should this have an Ord constraint?
reifyFn :: (FakeFinitary a, Eq a) => FTMap a b -> Map a b
reifyFn (FTMap f) = M.fromAscList [(a, f a) | a <- inhabitants]

-- unsafe because the map isn't required to be total. 
unsafeUnreify :: (Ord a, FakeFinitary a) => Map a b -> FTMap a b
unsafeUnreify m = FTMap (m M.!)

instance (Eq a, Eq b, FakeFinitary a) => Eq (FTMap a b) where
    (==) f g = reifyFn f == reifyFn g

instance (Show a, Show b, FakeFinitary a, Eq a) => Show (FTMap a b) where
    show f = show (reifyFn f)

instance (Eq a, FakeFinitary a) => Foldable (FTMap a) where
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

instance (FakeFinitary a, Ord a, Ord b) => Ord (FTMap a b) where
    compare f g = compare (reifyFn f) (reifyFn g)

-- TODO: change to !!! and import qualified
(!!!) :: FTMap a b -> a -> b
(!!!) = runFn

applyAt :: Eq a => a -> (b -> b) -> FTMap a b -> FTMap a b
applyAt a fn f = FTMap (\x -> if x == a then fn (runFn f x) else runFn f x)

-- TODO: is this better with some kind of strictness marking?
-- probably not significant for this use case
-- update (a,b) = applyAt (a, const b)
update :: Eq a => (a,b) -> FTMap a b  -> FTMap a b
update (a,b) f = FTMap (\x -> if x == a then b else runFn f x)

filter :: (FakeFinitary a, Eq a) => (b -> Bool) -> FTMap a b -> Map a b
filter filt = M.filter filt .  reifyFn

-- Lenses

-- This is not actually an isomorphism. It's a map with a section, i.e. an embedding.
-- Unsafe stuff could happen on the map side.

ftIso :: (FakeFinitary a, Ord a) =>  Iso' (FTMap a b) (Map a b)
ftIso = iso reifyFn unsafeUnreify

ftAt :: (Eq a, Functor f) => a -> (b -> f b) -> FTMap a b -> f (FTMap a b)
ftAt x = lens (!!! x) (\f y -> update (x,y) f)






