{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import CantStop (initGameState, csRunPlay, csVisibility, cantStop)
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
import Objects (CantStopGameState, CantStopLocation, CantStopCounterName, CantStopResource, CantStopPhaseName, PlayName, Issue, CSView, CantStopOptions)
import Effectful (Eff, (:>), IOE, liftIO)
import Effectful.Dispatch.Dynamic (interpret)
import Game.Run (runGameChannels)
import qualified Debug.Trace as Debug
import Control.Lens ((^.))

parsePayload :: Either CSView CantStopOptions -> BEvent
parsePayload (Left csv) = Receive csv
parsePayload (Right opts) = Request opts

main :: IO ()
main = do
    let gs = initGameState 3
    let gsv = viewGameStateAs'  gs (Player 0)
    brickToGameChan <- newChan
    gameToBrickChan <- newChan
    gameToBrickBChan <- newBChan 100
    brickToGameBChan <- newBChan 100
    initVty <- V.mkVty V.defaultConfig


    -- setup Game -> Brick channel
    forkIO $ forever $ do
        payload <- readChan gameToBrickChan
        let parsed = parsePayload payload
        writeBChan gameToBrickBChan parsed

    -- setup Brick -> Game channel 
    forkIO $ forever $ do
        payload' <- readBChan brickToGameBChan
        writeChan brickToGameChan payload'

    forkIO $ forever $
        runGameChannels (Player 0) 
            (cantStop 3 ^. #gameState) 
            (cantStop 3 ^. #playRunner,  cantStop 3 ^. #setup, cantStop 3 ^. #phases)
            gameToBrickChan 
            brickToGameChan
    let initTUI = TUIState gsv (Player 0) Nothing ShowState brickToGameBChan
    void $ customMain initVty (V.mkVty V.defaultConfig) (Just gameToBrickBChan) app initTUI

