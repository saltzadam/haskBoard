{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import CantStop (initGameState, csRunPlay, cantStop)
import Brick (defaultMain, customMain)
import Tui (app, drawBoardView, BEvent (..), TUIState (..), TUIMode (..))
import Brick.Main (simpleMain)
import Brick.BChan (newBChan, writeBChan, BChan, readBChan)
import Game.View ( viewGameStateAs', buildView')
import Game.Player (Player(..))
import Game.Visibility (allVisible)
import Control.Concurrent (forkIO, newChan, readChan, writeChan)
import Control.Monad (forever, void)
import qualified Graphics.Vty as V
import GHC.Conc (threadDelay)
import Objects (CantStopGameState, CantStopLocation, CantStopCounterName, CantStopResource, CantStopPhaseName, CantStopPlayName, CantStopIssue, CSView, CantStopOptions, CantStopLocations, CantStopCounters)
import Effectful (Eff, (:>), IOE, liftIO)
import Effectful.Dispatch.Dynamic (interpret)
import Game.Run (runGameCommonChannels, runGameFromInterfaces)
import qualified Debug.Trace as Debug
import Control.Lens ((^.))
import Game.Choose (GameToInterfacePayload (..))
import Control.Concurrent (Chan(..))
import qualified Data.Map as M
import Agent (brickAgent, randomAgent)
import Game.Agent (runAgentIO, runFromAgentIO)
import Game.Controller (agentToInterface, GameController (..))
import qualified Data.Set as S

parsePayload :: GameToInterfacePayload CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName CantStopIssue -> BEvent
parsePayload (SendState csv) = Receive csv
parsePayload (SendOptions gsv opts) = Request opts
parsePayload (SendWinners winners) = AnnounceWinner winners


sendToBrickBChan :: Chan (GameToInterfacePayload CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName CantStopIssue) -> BChan  BEvent -> IO ()
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

    let controller = agentToInterface <$> M.fromList [(Player 1, playerAgent), (Player 2, ai1), (Player 3, ai2)]

    initVty <- V.mkVty V.defaultConfig
    forkIO $
        void $ runGameFromInterfaces 
            (cantStop 3 ^. #gameState)
            (cantStop 3 ^. #playRunner,  cantStop 3 ^. #setup, cantStop 3 ^. #phases, cantStop 3 ^. #score)
            (GameController controller)

    let gsv = viewGameStateAs'  gs (Player 0)
    let initTUI = TUIState gsv (Player 0) ShowState [] brickToGameBChan Nothing True
    void $ customMain initVty (V.mkVty V.defaultConfig) (Just gameToBrickBChan) app initTUI



-- main :: IO ()
-- main = do
--     let gs = initGameState 3
--     let gsv = viewGameStateAs'  gs (Player 0)

--     brickToGameChan <- newChan
--     gameToBrickChan <- newChan
--     gameToBrickBChan <- newBChan 100
--     brickToGameBChan <- newBChan 100
--     initVty <- V.mkVty V.defaultConfig


--     -- setup Game -> Brick channel
--     forkIO $ forever $ sendToBrickBChan gameToBrickChan gameToBrickBChan

--     -- setup Brick -> Game channel 
--     forkIO $ forever $ getFromBrickBChan brickToGameBChan brickToGameChan

--     forkIO $
--         void $ runGameCommonChannels (Player 0)
--             (cantStop 3 ^. #gameState)
--             (cantStop 3 ^. #playRunner,  cantStop 3 ^. #setup, cantStop 3 ^. #phases, cantStop 3 ^. #score)
--             gameToBrickChan
--             brickToGameChan
--     let initTUI = TUIState gsv (Player 0) ShowState [] brickToGameBChan Nothing True
--     void $ customMain initVty (V.mkVty V.defaultConfig) (Just gameToBrickBChan) app initTUI

