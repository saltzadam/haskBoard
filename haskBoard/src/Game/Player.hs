{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Game.Player where

import GHC.Generics (Generic)
import Data.Finitary
import GHC.Word (Word8)

newtype Player = Player {num :: Word8} deriving (Eq, Ord, Show, Generic)
    deriving anyclass Finitary





somePlayers :: [Player]
somePlayers = [Player 0,
               Player 1,
               Player 2,
               Player 3]
