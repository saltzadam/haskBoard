{-# LANGUAGE DeriveGeneric #-}

module Game.Player where

import GHC.Generics (Generic)
import GHC.Word (Word8)

newtype Player = Player {num :: Word8} deriving (Eq, Ord, Show, Generic, Read, Bounded)

instance Enum Player where
    toEnum = Player . toEnum
    fromEnum (Player num) = fromEnum num





somePlayers :: [Player]
somePlayers = [Player 0,
               Player 1,
               Player 2,
               Player 3]
