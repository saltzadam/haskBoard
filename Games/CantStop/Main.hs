{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Main where

import Agent (CSEvent)
import Brick (customMain, defaultMain)
import Brick.BChan (BChan, newBChan, readBChan, writeBChan)
import Brick.Game.Tui (TUIMode (..), TUIState (..))
import Brick.Main (simpleMain)
import CantStop (cantStop, csRunPlay', initGameState)
import Control.Concurrent (Chan (..), forkIO, killThread, newChan, readChan, writeChan)
import Control.Lens ((^.))
import Control.Monad (forever, void)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Debug.Trace as Debug
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import GHC.Conc (threadDelay)
import Game.Agent (BEvent (..))
import Game.Choose
import Game.Player (Player (..))
import Game.View (buildView', viewGameStateAs')
import Game.Visibility (allVisible)
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Interface.Agent
import Interface.Controller
import Objects (CSView, CantStopCounterName, CantStopCounters, CantStopGameState, CantStopLocation, CantStopLocations, CantStopOptions, CantStopPhaseName, CantStopPlayName, CantStopResource)
import Run (runGameCommonChannels, runGameFromInterfaces)
import Tui (app)

parsePayload :: GameToInterfacePayload CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName -> CSEvent
parsePayload (SendState csv) = Receive csv
parsePayload (SendOptions gsv opts) = Request opts
parsePayload (SendWinners winners) = AnnounceWinner winners

sendToBrickBChan :: Chan (GameToInterfacePayload CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName) -> BChan CSEvent -> IO ()
sendToBrickBChan gameToBrickChan gameToBrickBChan = do
  payload <- readChan gameToBrickChan
  let parsed = parsePayload payload
  writeBChan gameToBrickBChan parsed

getFromBrickBChan brickToGameBChan brickToGameChan = do
  payload' <- readBChan brickToGameBChan
  writeChan brickToGameChan payload'

buildPlayerChannels p = do
  chan1 <- newChan
  chan2 <- newChan
  return (p, (chan1, chan2))

main :: IO ()
main = do
  let gs = initGameState 3
  let players = S.toList (gs ^. #players)
  playChannels <- M.fromList <$> traverse buildPlayerChannels players

  gameToBrickBChan <- newBChan 100
  brickToGameBChan <- newBChan 100
  let playerAgent = brickAgent (fst $ playChannels M.! Player 1) gameToBrickBChan (snd $ playChannels M.! Player 1) brickToGameBChan

  let ai1 = uncurry randomAgent (playChannels M.! Player 2)
  let ai2 = uncurry randomAgent (playChannels M.! Player 3)
  ai1thread <- forkIO (runAgentIO ai1)
  ai2thread <- forkIO (runAgentIO ai2)
  playerthread <- forkIO (runAgentIO playerAgent)

  let controller = agentToInterface <$> M.fromList [(Player 1, playerAgent), (Player 2, ai1), (Player 3, ai2)]

  initVty <- mkVty V.defaultConfig
  gameThread <-
    forkIO $
      void $
        runGameFromInterfaces
          (initGameState 3)
          cantStop
          (GameController controller)

  let gsv = viewGameStateAs' gs (Player 1)
  let initTUI = TUIState gsv (Player 1) ShowState [] brickToGameBChan Nothing True []
  void $ customMain initVty (mkVty V.defaultConfig) (Just gameToBrickBChan) app initTUI
  killThread ai1thread
  killThread ai2thread
  killThread playerthread
  killThread gameThread
