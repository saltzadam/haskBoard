{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Game where

import Control.Lens (makeFields)
import GHC.Generics (Generic)
import Game.Player (Player)
import Location (GameObjects)
import Control.Monad.Random (StdGen)

data Game lnames resources phase = Game
  { players :: [Player],
    objects :: GameObjects lnames resources,
    phaseStack :: [phase], -- provisional
    activePlayer :: Maybe Player,
    randGen :: StdGen
  }
  deriving (Generic)



makeFields ''Game
