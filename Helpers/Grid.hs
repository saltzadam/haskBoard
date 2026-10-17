{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Grid
  ( -- * Coordinates
    Coord (..),
    -- * Square cells
    Cell (..),
    offset,
    neighbors4,
    neighbors8,
  )
where

import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey)
import Data.Finitary (Finitary)
import Data.Finite (Finite, getFinite, packFinite)
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat, type (*))

-- | Coordinates with a notion of adjacency. Square cells use orthogonal
-- adjacency; hex coordinates (future) will have six neighbours.
class (Finitary c, Ord c) => Coord c where
  neighbors :: c -> [c]

-- | A square on an m-row, n-column board. Rows and columns are 0-indexed.
data Cell (m :: Nat) (n :: Nat) = Cell (Finite m) (Finite n)
  deriving (Eq, Ord, Show, Generic)

-- | Generic deriving enumerates row-major, so 'inhabitants' agrees with the
-- derived 'Ord' ('FinitaryMap.reifyFn' relies on that; tested).
-- @KnownNat (m * n)@ is in the context so we don't need the knownnat plugin.
deriving anyclass instance (KnownNat m, KnownNat n, KnownNat (m * n)) => Finitary (Cell m n)

-- | Serialised as @[row, col]@.
instance ToJSON (Cell m n) where
  toJSON (Cell r c) = toJSON (getFinite r, getFinite c)

instance (KnownNat m, KnownNat n) => FromJSON (Cell m n) where
  parseJSON v = do
    (r, c) <- parseJSON v
    maybe (fail "Cell: out of bounds") pure (Cell <$> packFinite r <*> packFinite c)

instance ToJSONKey (Cell m n)

instance (KnownNat m, KnownNat n) => FromJSONKey (Cell m n)

instance (KnownNat m, KnownNat n, KnownNat (m * n)) => Coord (Cell m n) where
  neighbors = neighbors4

-- | Move by (rows, cols). 'Nothing' if the result is off the board.
offset :: (KnownNat m, KnownNat n) => (Int, Int) -> Cell m n -> Maybe (Cell m n)
offset (dr, dc) (Cell r c) =
  Cell <$> packFinite (getFinite r + toInteger dr) <*> packFinite (getFinite c + toInteger dc)

-- | Orthogonally adjacent cells (von Neumann neighbourhood).
neighbors4 :: (KnownNat m, KnownNat n) => Cell m n -> [Cell m n]
neighbors4 cell = mapMaybe (`offset` cell) [(-1, 0), (0, -1), (0, 1), (1, 0)]

-- | Orthogonally and diagonally adjacent cells (Moore neighbourhood).
neighbors8 :: (KnownNat m, KnownNat n) => Cell m n -> [Cell m n]
neighbors8 cell = mapMaybe (`offset` cell) [(dr, dc) | dr <- [-1, 0, 1], dc <- [-1, 0, 1], (dr, dc) /= (0, 0)]
