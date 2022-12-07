{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Game where

import Control.Lens (makeFields)
import GHC.Generics (Generic)
import Game.Player (Player)
import Location (GameObjects)

data Game onames unames snames resources phase = Game
  { players :: [Player],
    locations :: GameObjects onames unames snames resources,
    phaseStack :: [phase], -- provisional
    activePlayer :: Maybe Player
  }
  deriving (Generic)

makeFields ''Game
