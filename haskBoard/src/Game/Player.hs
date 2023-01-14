{-# LANGUAGE DeriveGeneric #-}

module Game.Player where

import GHC.Generics (Generic)

data Player = Player {id :: Int, name :: String} deriving (Eq, Ord, Show, Generic)

somePlayers :: [Player]
somePlayers = [Player 0 "Justin",
               Player 1 "Saltz",
               Player 2 "Schwaid",
               Player 3 "Uri"]
