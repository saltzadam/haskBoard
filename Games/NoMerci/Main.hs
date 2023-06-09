{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}
module Main where
import NoMerci (initGameState, nmRunPlay, noMerci)
import Brick (defaultMain, customMain)
import Tui (app, drawBoardView, TUIState (..), TUIMode (..))
import Brick.Main (simpleMain)
import Brick.BChan (newBChan, writeBChan, BChan, readBChan)
import Game.View ( viewGameStateAs', buildView')
import Game.Player (Player(..))
import Game.Visibility (allVisible)
import Control.Concurrent
    ( forkIO, newChan, readChan, writeChan, Chan(..) )
import Control.Monad (forever, void)
import qualified Graphics.Vty as V
import GHC.Conc (threadDelay)
import Objects (NMGameState, NMLocation, NMResource, NMPhaseName, NMPlayName, NMIssue, NMView, NMOptions, NMLocation, NMCounters)
import Effectful (Eff, (:>), IOE, liftIO)
import Effectful.Dispatch.Dynamic (interpret)
import Game.Run (runGameCommonChannels, runGameFromInterfaces)
import qualified Debug.Trace as Debug
import Control.Lens ((^.))
import Game.Choose (GameToInterfacePayload (..))
import qualified Data.Map as M
import Game.Agent (runAgentIO, runFromAgentIO, randomAgent, brickAgent, BEvent(..))
import Game.Controller (agentToInterface, GameController (..))
import qualified Data.Set as S
import Agent (NMEvent)

parsePayload :: GameToInterfacePayload NMLocation NMCounters NMResource NMPhaseName NMPlayName NMIssue -> NMEvent
parsePayload (SendState csv) = Receive csv
parsePayload (SendOptions gsv opts) = Request opts
parsePayload (SendWinners winners) = AnnounceWinner winners


sendToBrickBChan :: Chan (GameToInterfacePayload NMLocation NMCounters NMResource NMPhaseName NMPlayName NMIssue) -> BChan  NMEvent -> IO ()
sendToBrickBChan gameToBrickChan gameToBrickBChan = do
    payload <- readChan gameToBrickChan
    let parsed = parsePayload payload
    writeBChan gameToBrickBChan parsed

getFromBrickBChan  brickToGameBChan brickToGameChan  = do
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
    playChannels <- M.fromList <$> traverse buildPlayerChannels  players

    gameToBrickBChan <- newBChan 100
    brickToGameBChan <- newBChan 100
    let playerAgent = brickAgent (fst $ playChannels M.! Player 1) gameToBrickBChan (snd $ playChannels M.! Player 1) brickToGameBChan

    let ai1 = uncurry randomAgent (playChannels M.! Player 2)
    let ai2 = uncurry randomAgent (playChannels M.! Player 3)
    forkIO (runFromAgentIO ai1)
    forkIO (runFromAgentIO ai2)
    forkIO (runFromAgentIO playerAgent)

    -- let controller = agentToInterface <$> M.fromList [(Player 1, playerAgent), (Player 2, ai1), (Player 3, ai2)]

    initVty <- V.mkVty V.defaultConfig
    forkIO $
        -- void $ runGameFromInterfaces
        void $ runGameCommonChannels (Player 1)
            (fst . noMerci $ 3)
            (snd . noMerci $ 3)
            -- (GameController controller)
            (playerAgent ^. #fromGameChannel)
            (playerAgent ^. #toGameChannel)
    let gsv = viewGameStateAs'  gs (Player 1)
    let initTUI = TUIState gsv (Player 1) ShowState [] brickToGameBChan Nothing True
    void $ customMain initVty (V.mkVty V.defaultConfig) (Just gameToBrickBChan) app initTUI


