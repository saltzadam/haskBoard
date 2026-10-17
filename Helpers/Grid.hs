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
    -- * Grids of locations
    Grid (..),
    cellLoc,
    -- * Queries
    lookCell,
    cellHas,
    occupied,
    findPiece,
    findPieces,
    snapshot,
    -- * Rules
    moveCell,
    placeAt,
    removeFrom,
  )
where

import Control.Monad (filterM)
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey)
import Data.Finitary (Finitary, inhabitants)
import Data.Finite (Finite, getFinite, packFinite)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import FinitaryMap (FTMap (..), (!!!))
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat, type (*))
import Game.Location (LocationShape)
import Game.Rules (GameRule, QueryM, lookLocation)
import Helpers (has, transfer, whatsAt)

-- | Coordinates with a notion of adjacency. 
class (Finitary c, Ord c) => Coord c where
  neighbors :: c -> [c]

-- | A square on an m-row, n-column board. Rows and columns are 0-indexed.
data Cell (m :: Nat) (n :: Nat) = Cell (Finite m) (Finite n)
  deriving (Eq, Ord, Show, Generic)

-- | Generic deriving enumerates row-major, so the order of 'inhabitants' agrees with
-- derived 'Ord'.
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

-- | Orthogonally adjacent cells
neighbors4 :: (KnownNat m, KnownNat n) => Cell m n -> [Cell m n]
neighbors4 cell = mapMaybe (`offset` cell) [(-1, 0), (0, -1), (0, 1), (1, 0)]

-- | Orthogonally and diagonally adjacent cells
neighbors8 :: (KnownNat m, KnownNat n) => Cell m n -> [Cell m n]
neighbors8 cell = mapMaybe (`offset` cell) [(dr, dc) | dr <- [-1, 0, 1], dc <- [-1, 0, 1], (dr, dc) /= (0, 0)]

-- | Names the game location for each cell of a board. @c@ is usually
-- @'Cell' m n@, but other tilings could be used.
newtype Grid c l = Grid (FTMap c l)

cellLoc :: Grid c l -> c -> l
cellLoc (Grid f) = (f !!!)

lookCell :: (QueryM l cn r ph pl m, Eq l) => Grid c l -> c -> m (LocationShape r)
lookCell g = lookLocation . cellLoc g

cellHas :: (QueryM l cn r ph pl m, Ord r, Eq l) => Grid c l -> c -> r -> m Bool
cellHas g = has . cellLoc g

occupied :: (QueryM l cn r ph pl m, Ord r, Eq l) => Grid c l -> c -> m Bool
occupied g cell = not . S.null <$> whatsAt (cellLoc g cell)

-- | Cells holding the piece, in 'inhabitants' order. Scans every cell.
findPiece :: (QueryM l cn r ph pl m, Finitary c, Ord r, Eq l) => Grid c l -> r -> m [c]
findPiece g r = filterM (\cell -> cellHas g cell r) inhabitants

findPieces :: (QueryM l cn r ph pl m, Finitary c, Ord r, Eq l) => Grid c l -> [r] -> m [c]
findPieces g rs = concat <$> traverse (findPiece g) rs

-- | The contents of every cell.
snapshot :: (QueryM l cn r ph pl m, Ord c, Finitary c, Eq l) => Grid c l -> m (FTMap c (LocationShape r))
snapshot (Grid f) = traverse lookLocation f

moveCell :: Grid c l -> c -> c -> r -> GameRule l cn r ph pl ()
moveCell g from to = transfer (cellLoc g from) (cellLoc g to)

placeAt :: l -> Grid c l -> c -> r -> GameRule l cn r ph pl ()
placeAt source g cell = transfer source (cellLoc g cell)

removeFrom :: Grid c l -> c -> l -> r -> GameRule l cn r ph pl ()
removeFrom g cell = transfer (cellLoc g cell)
