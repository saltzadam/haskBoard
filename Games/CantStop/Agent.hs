{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Agent where

import Brick.BChan (BChan, readBChan, writeBChan)
import Control.Concurrent (Chan)
import Game.Agent
import Game.Choose (GameToInterfacePayload)
import Game.Player (Player)
import Objects
import System.Random (uniformR, uniform, randomRIO)
import Game.Options (Options(..))
import qualified Data.List.NonEmpty as NE

type CSAgent = Agent CantStopLocations CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName CantStopIssue IO
type CSPayload = GameToInterfacePayload 
       CantStopLocation
       CantStopCounterName
       CantStopResource
       CantStopPhaseName
       CantStopPlayName
       CantStopIssue
type CSEvent = BEvent
       CantStopLocation
       CantStopCounterName
       CantStopResource
       CantStopPhaseName
       CantStopPlayName
       CantStopIssue
