{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Agent where

import Brick.BChan (BChan, readBChan, writeBChan)
import Control.Concurrent (Chan)
import Game.Agent
import Game.Choose (GameToInterfacePayload)
import Game.Player (Player)
import Objects
import Tui (BEvent (..))
import System.Random (uniformR, uniform, randomRIO)
import Game.Options (Options(..))
import qualified Data.List.NonEmpty as NE

type CSAgent = Agent CantStopLocations CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName CantStopIssue IO

brickAgent ::
  Chan
    ( GameToInterfacePayload
        CantStopLocation
        CantStopCounterName
        CantStopResource
        CantStopPhaseName
        CantStopPlayName
        CantStopIssue
    ) ->
  BChan BEvent ->
  Chan CantStopPlayName ->
  BChan CantStopPlayName ->
  Agent
    CantStopLocation
    CantStopCounterName
    CantStopResource
    CantStopPhaseName
    CantStopPlayName
    CantStopIssue
    IO
brickAgent fromGameChan toBrickBChan toGameChan fromBrickBChan =
  Agent
    { playChooser = \ _ options -> do
        writeBChan toBrickBChan (Request options)
        readBChan fromBrickBChan,
      stateHandler = \gsv -> writeBChan toBrickBChan (Receive gsv),
      winnersHandler = \winners -> writeBChan toBrickBChan (AnnounceWinner winners),
      fromGameChannel = fromGameChan,
      toGameChannel = toGameChan
    }
--
-- chooses moves at random
randomAgent ::
  Chan
    ( GameToInterfacePayload
        CantStopLocation
        CantStopCounterName
        CantStopResource
        CantStopPhaseName
        CantStopPlayName
        CantStopIssue
    ) ->
  Chan CantStopPlayName ->
  Agent
    CantStopLocation
    CantStopCounterName
    CantStopResource
    CantStopPhaseName
    CantStopPlayName
    CantStopIssue
    IO
randomAgent fromGameChan toGameChan = Agent {
    playChooser = chooseRandom,
    stateHandler = \_ -> return (),
    winnersHandler = \_ -> return (),
    fromGameChannel = fromGameChan,
    toGameChannel = toGameChan

                                                                        }


chooseRandom ::  p -> Options b i -> IO b
chooseRandom _ (Options legal _ _) = let
    numOptions = length legal
  in do
      choice <- randomRIO (1, numOptions)
      return (legal NE.!! (choice - 1))
