{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Game.Player where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Finitary (Finitary)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import GHC.Generics (Generic)

data PlayerNum = PlayerOne | PlayerTwo | PlayerThree | PlayerFour | PlayerFive | PlayerSix
  deriving (Eq, Ord, Generic, Finitary, Show, Read, Bounded, Enum, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

displayPlayerNum :: PlayerNum -> String
displayPlayerNum PlayerOne = "Player One"
displayPlayerNum PlayerTwo = "Player Two"
displayPlayerNum PlayerThree = "Player Three"
displayPlayerNum PlayerFour = "Player Four"
displayPlayerNum PlayerFive = "Player Five"
displayPlayerNum PlayerSix = "Player Six"

displayPlayer :: Player -> String
displayPlayer (Player pnum) = displayPlayerNum pnum

displayPlayerT :: Player -> T.Text
displayPlayerT = T.pack . displayPlayer

newtype Player = Player {num :: PlayerNum} deriving (Eq, Ord, Show, Generic, Read, Bounded, Finitary, Enum, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

-- for convenience
instance Num PlayerNum where
  fromInteger = toEnum . subtract 1 . fromIntegral

mkPlayers :: Int -> [Player]
mkPlayers i = Player <$> [toEnum 0 .. toEnum (i - 1)]

data Turn phaseName = Turn
  { owner :: Player,
    turnPhases :: NE.NonEmpty phaseName
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
