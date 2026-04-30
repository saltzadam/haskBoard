{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module FinitaryMap where

-- TODO: weird performance regression depending on imports because of inlining
-- ( FTMap (..),
--   (!!!),
--   applyAt,
--   update,
--   -- , fAt
--   -- , fApplyAt
--   ftAt,
-- )

import Control.Lens (lens)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson.Types (FromJSONKey, ToJSONKey)
import Data.Digits (digits, unDigits)
import Data.Finitary (Finitary (..), inhabitants)
import Data.Finite (Finite, finite, getFinite)
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)
import GHC.TypeNats (type (^))
import Prelude

-- FTMap is a "Finitary, total map". That is, it's a total map from a finitary domain.
-- It's represented as a newtype wrapper on `a -> b` where a is Finitary.

newtype FTMap a b = FTMap {runFn :: a -> b} deriving (Generic, Functor, Applicative)

-- instance (Finitary a, Eq a, FromJSON a, FromJSON b) => FromJSON (FTMap a b)
instance (Finitary a, Eq a, Ord a, ToJSON a, ToJSON b, ToJSONKey a) => ToJSON (FTMap a b) where
  toJSON = toJSON . reifyFn

instance (Ord a, Finitary a, FromJSONKey a, FromJSON b) => FromJSON (FTMap a b) where
  parseJSON = fmap unsafeUnreify . parseJSON

instance (Finitary a, Finitary b, Ord a) => Finitary (FTMap a b) where
  type Cardinality (FTMap a b) = Cardinality a ^ Cardinality b
  toFinite (FTMap f) = finite . fromIntegral . unDigits (length (inhabitants :: [b])) $ fromIntegral . getFinite . toFinite . f <$> inhabitants
  fromFinite i = unsafeUnreify $ M.fromAscList $ zip (inhabitants :: [a]) $ fmap (fromFinite . finite . fromIntegral :: Int -> b) $ padList (length (inhabitants :: [a])) 0 $ digits (length (inhabitants :: [b])) (fromIntegral $ getFinite i)

-- fns :: [FTMap a b]
-- fns = unsafeUnreify <$> M.fromList [(a,

-- maps :: (Finitary a, Finitary b, Ord a) => Map a [b]
-- maps = M.fromList [(a, inhabitants) | a <- inhabitants]

-- fns :: (Ord a, Finitary a, Finitary b) => [FTMap a b]
-- fns = unsafeUnreify <$> sequence maps

-- g :: forall a b. (Finitary a, Finitary b) => (a -> b) -> Int
-- g f = unDigits base $ toInt . f <$> inhabitants
--   where
--     toInt = fromIntegral . getFinite . toFinite
--     base = length (inhabitants :: [b])

padList :: Int -> a -> [a] -> [a]
padList targetLength def xs = replicate (targetLength - length xs) def ++ xs

-- h :: forall a b. (Finitary a, Finitary b, Ord a) => Int -> (FTMap a b)
-- h i = unsafeUnreify $ M.fromAscList $ zip (inhabitants :: [a]) $ fmap (fromFinite . finite . fromIntegral :: Int -> b) $ padList (length (inhabitants :: [a])) 0 $ digits (length (inhabitants :: [b])) i

-- From function to Map using the finitary-ness of `a`.
-- TODO: change doc
reifyFn :: (Finitary a, Eq a) => FTMap a b -> Map a b
reifyFn (FTMap f) = M.fromAscList [(a, f a) | a <- inhabitants]

resultsFn :: (Finitary a) => FTMap a b -> [b]
resultsFn (FTMap f) = f <$> inhabitants

-- unsafe because the map isn't required to be total.
unsafeUnreify :: (Ord a, Finitary a) => Map a b -> FTMap a b
unsafeUnreify m = FTMap (m M.!)

instance (Eq a, Eq b, Finitary a) => Eq (FTMap a b) where
  (==) f g = resultsFn f == resultsFn g

instance (Show a, Show b, Finitary a, Eq a) => Show (FTMap a b) where
  show = show . reifyFn

instance (Eq a, Finitary a) => Foldable (FTMap a) where
  foldMap g = foldMap g . reifyFn

instance (Semigroup b) => Semigroup (FTMap a b) where
  (<>) = liftA2 (<>)

instance (Monoid b) => Monoid (FTMap a b) where
  mempty = FTMap (const mempty)

instance (Num b) => Num (FTMap a b) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger i = FTMap (const (fromInteger i))

instance (Finitary a, Ord a, Ord b) => Ord (FTMap a b) where
  compare f g = compare (resultsFn f) (resultsFn g)

(!!!) :: FTMap a b -> a -> b
(!!!) (FTMap f) = f

applyAt :: (Eq a) => a -> (b -> b) -> FTMap a b -> FTMap a b
applyAt a fn f = FTMap (\x -> if x == a then fn (f !!! x) else f !!! x)

update :: (Eq a) => (a, b) -> FTMap a b -> FTMap a b
update (a, b) = applyAt a (const b)

-- Lenses

ftAt :: (Eq a, Functor f) => a -> (b -> f b) -> FTMap a b -> f (FTMap a b)
ftAt x = lens (!!! x) (\f y -> update (x, y) f)

-- fAt :: (Functor f, Eq a) => a -> (b -> f b) -> (a -> b) -> f (a -> b)
-- fAt x = lens ($ x) (\f y z -> if z == x then y else f x)

-- applyAt :: Eq a => a -> (b -> b) -> FTMap a b -> FTMap a b
-- applyAt a fn f = FTMap (\x -> if x == a then fn (f !!! x) else f !!! x)

-- fApplyAt a fn f x = if x == a then fn (f x) else f x
