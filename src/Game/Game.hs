{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Game.Game where
import Game.Player ( Player )
import Location ( GameObjects )
import GHC.Generics ( Generic )
import Control.Lens ( makeFields )

data Game onames unames snames resources phase = Game
  { players :: [Player],
    locations :: GameObjects onames unames snames resources,
    phaseStack :: [phase], -- provisional
    activePlayer :: Maybe Player
  }
  deriving (Generic)

makeFields ''Game


