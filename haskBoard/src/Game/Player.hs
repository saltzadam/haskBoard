{-# LANGUAGE DeriveGeneric #-}

module Game.Player where

import GHC.Generics (Generic)
import Data.Finitary

newtype Player = Player {id :: Int} deriving (Eq, Ord, Show, Generic)

instance Finitary Player


somePlayers :: [Player]
somePlayers = [Player 0,
               Player 1,
               Player 2,
               Player 3]
