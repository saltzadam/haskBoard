{-# LANGUAGE DeriveAnyClass #-}

-- | Thin module containing only the 'GameState' data type.
-- Kept separate so that 'Game.Rules' can reference 'GameState'
-- without creating a circular import with 'Game.GameState'.
module Game.GameStateBase
  ( GameState (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Set (Set)
import GHC.Generics (Generic)
import Game.Location (GameObjects)
import Game.Player (Player, Turn)
import Game.Visibility (VisibilityMap)

-- | The full mutable state of a game in progress.
data GameState l cn r ph pl = GameState
  { players :: Set Player,
    objects :: GameObjects l cn r,
    currentPhase :: ph,
    currentTurn :: Turn ph,
    nextTurn :: Maybe (Turn ph),
    visibility :: VisibilityMap l cn
  }
  deriving (Generic, FromJSON, ToJSON)
