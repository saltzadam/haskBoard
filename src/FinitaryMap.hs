{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module FinitaryMap where
import Prelude hiding (lookup)
import Data.Finitary
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)
import GHC.Base (liftA2)
import Control.Lens ( Contravariant, at, to, lens )
import Data.Maybe (fromJust)
import Control.Lens.Iso
import Control.Lens.Prism

newtype FTMap a b = FTMap {runFn :: a -> b} deriving (Generic, Functor, Applicative)

reifyFn :: Finitary a => FTMap a b -> Map a b
reifyFn (FTMap f) = M.fromAscList [(a, f a) | a <- inhabitants]

unsafeUnreify :: Ord a => Map a b -> FTMap a b
unsafeUnreify m = FTMap (m M.!)


instance (Eq a, Eq b, Finitary a) => Eq (FTMap a b) where
    (==) f g = reifyFn f == reifyFn g

instance (Show a, Show b, Finitary a) => Show (FTMap a b) where
    show f = show (reifyFn f)

instance Finitary a => Foldable (FTMap a) where
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

-- lookup :: FTMap a b -> a -> b
-- f `lookup` a = runFn f a

(!!!) :: FTMap a b -> a -> b
(!!!) = runFn

update :: Eq a => (a,b) -> FTMap a b  -> FTMap a b
update (a,b) f = FTMap (\x -> if x == a then b else runFn f x)

-- Lenses

-- This is not actually an isomorphism. It's a map with a section, i.e. an embedding.
-- Unsafe stuff could happen on the map side.

ftIso :: (Finitary a, Ord a) =>  Iso' (FTMap a b) (Map a b)
ftIso = iso reifyFn unsafeUnreify

ftAt :: (Eq a, Functor f) => a -> (b -> f b) -> FTMap a b -> f (FTMap a b)
ftAt x = lens (!!! x) (\f y -> update (x,y) f)


-- ftAt l = ftIso . at l . over fromJust




