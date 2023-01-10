{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}

module Count where

import Control.Applicative (liftA2)
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)
import System.Random.Stateful (UniformRange(uniformRM), Uniform (..))
import Defaultable.Map (Defaultable(..))
import Control.Monad.Random
import Data.Bifunctor (bimap)

-- This is Maybe a with the opposite order for Nothing
-- Use it for counting things, think about "unlimited" stuff
data Cnt a = Cnt a | Infinity deriving (Eq, Show, Functor, Generic, Finite)

instance Ord a => Ord (Cnt a) where
  (Cnt a) <= (Cnt b) = a <= b
  Infinity <= (Cnt _) = False
  (Cnt _) <= Infinity = True
  Infinity <= Infinity = True

instance Applicative Cnt where
  pure = Cnt
  (<*>) (Cnt f) (Cnt a) = Cnt (f a)
  (<*>) Infinity _ = Infinity
  (<*>) _ Infinity = Infinity

instance Semigroup a => Semigroup (Cnt a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Cnt a) where
  mempty = pure mempty

instance Num a => Num (Cnt a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)

  abs Infinity = Infinity
  abs (Cnt a) = Cnt (abs a)

  signum Infinity = 1
  signum (Cnt a) = Cnt (signum a)

  fromInteger = Cnt . fromInteger

  (-) Infinity _ = Infinity
  (-) _ Infinity = error "no negative infinity"
  (-) (Cnt a) (Cnt b) = Cnt ((-) a b)

infinityToMax :: Bounded a => Cnt a -> a
infinityToMax Infinity = maxBound
infinityToMax (Cnt a) = a

safeTuple :: (Bounded a, Bounded b) => (Cnt a, Cnt b) -> (a,b)
safeTuple = bimap infinityToMax infinityToMax

instance Uniform a => Uniform (Cnt a) where
    uniformM = fmap Cnt . uniformM

instance (Bounded a, UniformRange a) => UniformRange (Cnt a) where
    uniformRM lohi = fmap Cnt . uniformRM (safeTuple lohi)

instance Random (Cnt Int)

histogramF :: (Foldable f, Ord a) => f a -> Defaultable (Map a) (Cnt Int)
histogramF foldable = Defaultable (foldl' (flip (M.alter plusOrInsertOne)) mempty foldable) (Just 0)
  where
    plusOrInsertOne = Just . maybe 0 (+ 1)

countF :: (Foldable f, Eq a) => a -> f a -> Cnt Int
countF x = foldl (\acc a -> if a == x then acc + 1 else acc) 0
