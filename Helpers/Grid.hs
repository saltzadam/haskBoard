{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Grid
  ( -- * Square cells
    Cell (..),
  )
where

import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey)
import Data.Finitary (Finitary)
import Data.Finite (Finite, getFinite, packFinite)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat, type (*))

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
