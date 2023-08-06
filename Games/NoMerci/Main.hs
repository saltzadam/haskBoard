{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use uncurry" #-}
module Main where

import Agent (NMEvent)
import Brick (customMain, defaultMain)
import Brick.BChan (BChan, newBChan, readBChan, writeBChan)
import Brick.Game.Tui (TUIMode (..), TUIState (..))
import Brick.Main (simpleMain)
import Control.Concurrent
  ( Chan (..),
    forkIO,
    newChan,
    readChan,
    writeChan,
  )
import Control.Lens ((^.))
import Control.Monad (forever, void)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Debug.Trace as Debug
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import GHC.Conc (threadDelay)
import Game.Agent (BEvent (..))
import Game.Choose (GameToInterfacePayload (..))
import Game.Player (Player (..))
import Game.View (buildView', viewGameStateAs')
import Game.Visibility (allVisible)
import qualified Graphics.Vty as V
import Interface.Agent
import NoMerci
import Objects (NMCounters, NMGameState, NMIssue, NMLocation, NMOptions, NMPhaseName, NMPlayName, NMResource, NMView)
import Run (runGameCommonChannels, runGameFromInterfaces)
import Tui (app)

parsePayload :: GameToInterfacePayload NMLocation NMCounters NMResource NMPhaseName NMPlayName NMIssue -> NMEvent
parsePayload (SendState csv) = Receive csv
parsePayload (SendOptions gsv opts) = Request opts
parsePayload (SendWinners winners) = AnnounceWinner winners

sendToBrickBChan :: Chan (GameToInterfacePayload NMLocation NMCounters NMResource NMPhaseName NMPlayName NMIssue) -> BChan NMEvent -> IO ()
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
  let gs = fst (noMerci 3)
  let players = S.toList (gs ^. #players)
  playChannels <- M.fromList <$> traverse buildPlayerChannels players

  gameToBrickBChan <- newBChan 100
  brickToGameBChan <- newBChan 100
  let playerAgent = brickAgent (fst $ playChannels M.! Player 1) gameToBrickBChan (snd $ playChannels M.! Player 1) brickToGameBChan

  let ai1 = uncurry randomAgent (playChannels M.! Player 2)
  let ai2 = uncurry randomAgent (playChannels M.! Player 3)
  forkIO (runAgentIO ai1)
  forkIO (runAgentIO ai2)
  forkIO (runAgentIO playerAgent)

  -- let controller = agentToInterface <$> M.fromList [(Player 1, playerAgent), (Player 2, ai1), (Player 3, ai2)]

  initVty <- V.mkVty V.defaultConfig
  forkIO $
    -- void $ runGameFromInterfaces
    void $
      runGameCommonChannels
        (Player 1)
        (fst . noMerci $ 3)
        (snd . noMerci $ 3)
        -- (GameController controller)
        (playerAgent ^. #fromGameChannel)
        (playerAgent ^. #toGameChannel)
  let gsv = viewGameStateAs' gs (Player 1)
  let initTUI = TUIState gsv (Player 1) ShowState [] brickToGameBChan Nothing True
  void $ customMain initVty (V.mkVty V.defaultConfig) (Just gameToBrickBChan) app initTUI
