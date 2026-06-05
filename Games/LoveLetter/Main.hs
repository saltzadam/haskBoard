{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use uncurry" #-}

module Main where

import Agent
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
import LoveLetter
import Objects
import Run (runGameCommonChannels, runGameFromInterfaces)
import Tui

parsePayload :: GameToInterfacePayload LLLocation LLCounters LLResource LLPhaseName LLPlayName LLIssue -> LLEvent
parsePayload (SendState csv) = Receive csv
parsePayload (SendOptions gsv opts) = Request opts
parsePayload (SendWinners winners) = AnnounceWinner winners

sendToBrickBChan :: Chan (GameToInterfacePayload LLLocation LLCounters LLResource LLPhaseName LLPlayName LLIssue) -> BChan LLEvent -> IO ()
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
  let gs = fst (loveLetter 3)
  let players = S.toList (gs ^. #players)
  playChannels <- M.fromList <$> traverse buildPlayerChannels players

  gameToBrickBChan <- newBChan 100
  brickToGameBChan <- newBChan 100
  let playerAgent = brickAgent (fst $ playChannels M.! Player 1) gameToBrickBChan (snd $ playChannels M.! Player 1) brickToGameBChan

  let ai1 = uncurry (randomAgent []) (playChannels M.! Player 2)
  let ai2 = uncurry (randomAgent []) (playChannels M.! Player 3)
  forkIO (runAgentIO ai1)
  forkIO (runAgentIO ai2)
  forkIO (runAgentIO playerAgent)

  initVty <- V.mkVty V.defaultConfig
  forkIO $
    -- void $ runGameFromInterfaces
    void $
      runGameCommonChannels
        (Player 1)
        (fst . loveLetter $ 3)
        (snd . loveLetter $ 3)
        -- (GameController controller)
        (playerAgent ^. #fromGameChannel)
        (playerAgent ^. #toGameChannel)
  let gsv = viewGameStateAs' gs (Player 1)
  let initTUI = TUIState gsv (Player 1) ShowState [] brickToGameBChan Nothing True []
  void $ customMain initVty (V.mkVty V.defaultConfig) (Just gameToBrickBChan) app initTUI
