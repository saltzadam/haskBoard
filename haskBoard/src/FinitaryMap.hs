{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FinitaryMap where
import Prelude hiding (lookup)
import Data.Finitary
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)
import GHC.Base (liftA2)
import Control.Lens ( lens )
import Control.Lens.Iso
import GHC.TypeNats
import Data.Finite.Internal ( Finite(..), getFinite, finite)
import Control.Monad (replicateM)
import Data.List (lookup)
import Data.Maybe (fromJust)

newtype FTMap a b = FTMap {runFn :: a -> b} deriving (Generic, Functor, Applicative)

reifyFn :: Finitary a => FTMap a b -> Map a b
reifyFn (FTMap f) = M.fromAscList [(a, f a) | a <- inhabitants]

unsafeUnreify :: Ord a => Map a b -> FTMap a b
unsafeUnreify m = FTMap (m M.!)

-- enumerateFn' :: forall a b . (Finitary a, Finitary b) => [([(a,b)], Integer)]
-- enumerateFn' = let lena = length (inhabitants @a)
--                 in zip
--                     [zip inhabitants values | values <- replicateM lena inhabitants]
--                     [1..]

-- unsafeUnEnumerate :: Eq a => [(a,b)] -> FTMap a b
-- unsafeUnEnumerate pairs = FTMap (\x -> fromJust $ lookup x pairs)

-- instance (Finitary a, Finitary b, KnownNat (Cardinality b ^ Cardinality a)) => Finitary (FTMap a b) where
--     type Cardinality (FTMap a b) = Cardinality b ^ Cardinality a
--     fromFinite i = unsafeUnEnumerate . fst $ (enumerateFn' !! fromInteger (getFinite i))
--     toFinite f = finite . fromJust $ (lookup (val f) enumerated :: Maybe Integer)
--         where
--             enumerated = enumerateFn' :: [([(a,b)], Integer)]
--             val f = M.toList . reifyFn $ f :: [(a,b)]


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

filter :: (Finitary a, Eq a) => (b -> Bool) -> FTMap a b -> Map a b
filter filt = M.filter filt .  reifyFn

filterKey :: Finitary a => (a -> Bool) -> FTMap a b -> Map a b
filterKey filt = M.filterWithKey (\k _ -> filt k) . reifyFn


-- Lenses

-- This is not actually an isomorphism. It's a map with a section, i.e. an embedding.
-- Unsafe stuff could happen on the map side.

ftIso :: (Finitary a, Ord a) =>  Iso' (FTMap a b) (Map a b)
ftIso = iso reifyFn unsafeUnreify

ftAt :: (Eq a, Functor f) => a -> (b -> f b) -> FTMap a b -> f (FTMap a b)
ftAt x = lens (!!! x) (\f y -> update (x,y) f)






