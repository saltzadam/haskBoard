{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Game.Player where

import Data.Finitary (Finitary)
import GHC.Generics (Generic)

data PlayerNum = PlayerOne | PlayerTwo | PlayerThree | PlayerFour | PlayerFive | PlayerSix
  deriving (Eq, Ord, Generic, Finitary, Show, Read, Bounded, Enum)

displayPlayerNum :: PlayerNum -> String
displayPlayerNum PlayerOne = "Player One"
displayPlayerNum PlayerTwo = "Player Two"
displayPlayerNum PlayerThree = "Player Three"
displayPlayerNum PlayerFour = "Player Four"
displayPlayerNum PlayerFive = "Player Five"
displayPlayerNum PlayerSix = "Player Six"

displayPlayer :: Player -> String
displayPlayer (Player pnum) = displayPlayerNum pnum

newtype Player = Player {num :: PlayerNum} deriving (Eq, Ord, Show, Generic, Read, Bounded, Finitary, Enum)

-- for convenience
instance Num PlayerNum where
  fromInteger = toEnum . subtract 1 . fromIntegral

mkPlayers :: Int -> [Player]
mkPlayers i = Player <$> [toEnum 0 .. toEnum i]
