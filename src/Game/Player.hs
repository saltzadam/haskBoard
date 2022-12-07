{-# LANGUAGE DeriveGeneric #-}
module Game.Player 
    where
import GHC.Generics (Generic)


data Player = Player {id :: Int, name :: String} deriving (Eq, Ord, Show, Generic)
