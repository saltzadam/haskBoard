{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import CantStop (initGameState, moreInterestingGameState, csRunPlay, csVisibility, runCSTurns)
import Brick (defaultMain, customMain)
import Tui (app, drawBoardView, BEvent (..), TUIState (..))
import Brick.Main (simpleMain)
import Brick.BChan (newBChan, writeBChan, BChan)
import Game.View ( viewGameStateAs', buildView')
import Game.Player (Player(..))
import Game.Visibility (allVisible)
import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import qualified Graphics.Vty as V
import GHC.Conc (threadDelay)
import Objects (CantStopGameState, CantStopLocation, CantStopCounterName, CantStopResource, CantStopPhaseName, PlayName, Issue, CSView)
import Effectful (Eff, (:>), IOE, liftIO)
import Effectful.Dispatch.Dynamic (interpret)
import Game.Choose (Choosing)


main :: IO ()
main = do
    let gsv = viewGameStateAs'  (initGameState 3) (Player 1)
    let initTui = TUIState gsv (Player 1) (Receive gsv)
    chan <- newBChan 100
    writeBChan chan (Receive gsv)
    initVty <- V.mkVty V.defaultConfig
    void $ customMain initVty (V.mkVty V.defaultConfig) (Just chan) app initTui

