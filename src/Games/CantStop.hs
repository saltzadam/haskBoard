{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}
module Games.CantStop where

import Util
import Game.Player (Player)
import Game.Game (Game)
import Data.Void (Void)
import GHC.Generics
import Game (Phase(..))
import Control.Monad.Random (RandomGen)
import Control.Monad.Random.Lazy (Rand)
import Control.Monad.Random (randomR)
import Control.Monad.Random


data TrackName = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Eleven | Twelve
    deriving (Eq, Ord, Show, Enum, Bounded)


toNum :: TrackName -> Int
toNum t = fromEnum t + 2

maxSlot :: TrackName -> Int
maxSlot t = toNum t + 1

type TrackHeight = Int

data Location = Slot TrackName TrackHeight | BoxTop deriving (Eq, Ord, Show, Generic)

data DieVal = ValOne | ValTwo | ValThree | ValFour | ValFive | ValSix deriving (Eq, Ord, Show, Generic, Enum)
diceFromNum :: Int -> DieVal
diceFromNum i = toEnum (i+1)
data Dice = Dice DieVal DieVal DieVal DieVal deriving (Eq, Ord, Show, Generic)

trackSlots :: [Location]
trackSlots = [Slot name height | name <- enumerateFromRoot, height <- [1..maxSlot name]] 

rollDice :: RandomGen g => Rand g Dice
rollDice = do 
    r1 <- diceFromNum <$> getRandomR (1,6)
    r2 <- diceFromNum <$> getRandomR (1,6)
    r3 <- diceFromNum <$> getRandomR (1,6)
    r4 <- diceFromNum <$> getRandomR (1,6)
    return (Dice r1 r2 r3 r4)

data Resource = PlayerMarker Player | TempMarker | RDice (Dice DieVal DieVal DieVal DieVal)

data PhaseName = Roll | PlayerTurn Player deriving (Eq, Ord, Show, Generic)

doNothing :: p1 -> p2 -> [a]
doNothing _ _ = []


type CantStopGame = Game Void Location Dice Marker PhaseName 
type CantStopPhase = Phase PhaseName Void Location Dice Marker

rollAction :: CantStopGame -> CantStopGame
rollAction = 





