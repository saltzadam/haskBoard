{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Agent where

import Game.Agent
import Game.Choose (GameToInterfacePayload)
import Objects

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
