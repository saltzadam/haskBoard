{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import CantStop (initGameState, moreInterestingGameState, csRunPlay, csVisibility, runCSTurns)
import Brick (defaultMain, customMain)
import Tui (app, drawBoardView)
import Brick.Main (simpleMain)
import Brick.BChan (newBChan, writeBChan, BChan)
import Game.View (viewGameAs', viewGameStateAs')
import Game.Player (Player(..))
import Game.Visibility (allVisible)
import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import qualified Graphics.Vty as V
import GHC.Conc (threadDelay)
import Objects (CantStopGameState, CantStopLocation, CantStopCounterName, CantStopResource, CantStopPhaseName, PlayName, Issue)
import Effectful (Eff, (:>), IOE, liftIO)
import Effectful.Dispatch.Dynamic (interpret)




main :: IO ()
main = do
    chan <- newBChan 100
    forkIO $ forever $ do 
        writeBChan chan 10
        threadDelay 100000
    let gs = initGameState 3
    initVty <- V.mkVty V.defaultConfig
    void $ customMain initVty (V.mkVty V.defaultConfig) (Just chan) app (viewGameStateAs'  gs allVisible (Player 1))


