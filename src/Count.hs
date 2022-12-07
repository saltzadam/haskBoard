{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}

module Count where

import Control.Applicative (liftA2)
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as M

-- import Data.Finitary

-- This is Maybe a with the opposite order for Nothing
-- Use it for counting things, think about "unlimited" stuff
data Cnt a = Cnt a | Infinity deriving (Eq, Show, Functor)

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

histogramF :: (Foldable f, Ord a) => f a -> Map a (Cnt Int)
histogramF = foldl' (flip (M.alter plusOrInsertOne)) M.empty
  where
    plusOrInsertOne = Just . maybe 0 (+ 1)

countF :: (Foldable f, Eq a) => a -> f a -> Cnt Int
countF x = foldl (\acc a -> if a == x then acc + 1 else acc) 0
