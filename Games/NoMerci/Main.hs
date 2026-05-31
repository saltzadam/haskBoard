{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use uncurry" #-}

module Main where

import Brick (customMainWithDefaultVty)
import Brick.BChan (newBChan)
import Brick.Game.Tui (TUIMode (..), TUIState (..))
import Control.Concurrent (MVar, forkIO, newMVar)
import Control.Concurrent.Async (withAsync)
import Control.Lens ((^.))
import Control.Monad (forM_, void)
import Data.List (delete, elemIndex)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Game.Player (Player (..))
import Game.View (viewGameStateAs')
import Interface.Agent (brickAgent, randomAgent, runAgentIO)
import Interface.Controller (PlayerInterface (..), buildInterface)
import Interface.Server (server, spawnAgileRLAgent)
import Interface.Stdio (runStdioAgent)
import Interface.Training (stdioTrainingLoop)
import NoMerci
import Run (runGameSeparateChannels)
import System.Environment (getArgs)
import Tui (app)

withWorker :: IO a -> IO a -> IO a
withWorker outer inner = withAsync outer $ const inner

main :: IO ()
main = do
  args <- getArgs
  case () of
    _ | "--stdio"     `elem` args -> runStdioMode
      | "--tui"       `elem` args -> runTUIMode
      | "--ws-agents" `elem` args ->
          let checkpoint = args !! succ (fromJust (elemIndex "--ws-agents" args))
              humanN     = maybe 0 read (lookup "--human-player" (zip args (drop 1 args)))
          in runWSAgentsMode checkpoint humanN
      | otherwise -> runServerMode

runServerMode :: IO ()
runServerMode = do
  let gs = fst (noMerci 3)
  let gr = snd (noMerci 3)
  let players = S.toList (gs ^. #players)
  interface <- buildInterface players
  withWorker
    (void $ runGameSeparateChannels "nomerci.log" interface gs gr)
    (server 3 gs interface)

runStdioMode :: IO ()
runStdioMode = do
  let gs = fst (noMerci 3)
  let gr = snd (noMerci 3)
  let players = S.toList (gs ^. #players)
  interface <- buildInterface players
  lock <- newMVar ()
  forM_ (M.toList (interface ^. #playerInterfaces)) $ \(p, PlayerInterface fromChan toChan) ->
    void $ forkIO $ runStdioAgent p lock players gr fromChan toChan
  stdioTrainingLoop (gs, gr) "training.log" interface

runTUIMode :: IO ()
runTUIMode = do
  let gs = fst (noMerci 3)
  let gr = snd (noMerci 3)
  let players = S.toList (gs ^. #players)
  interface <- buildInterface players
  let channels = fmap (\(PlayerInterface fromChan toChan) -> (fromChan, toChan)) (interface ^. #playerInterfaces)
  gameToBrickBChan <- newBChan 100
  brickToGameBChan <- newBChan 100
  let playerAgent = brickAgent (fst $ channels M.! Player 1) gameToBrickBChan (snd $ channels M.! Player 1) brickToGameBChan
  let ai1 = uncurry randomAgent (channels M.! Player 2)
  let ai2 = uncurry randomAgent (channels M.! Player 3)
  let gsv = viewGameStateAs' gs (Player 1)
  let initTUI = TUIState gsv (Player 1) ShowState [] brickToGameBChan Nothing True []
  withWorker (runAgentIO playerAgent)
    $ withWorker (runAgentIO ai1)
    $ withWorker (runAgentIO ai2)
    $ withWorker (void $ runGameSeparateChannels "nomerci.log" interface gs gr)
    $ void (customMainWithDefaultVty (Just gameToBrickBChan) app initTUI)

runWSAgentsMode :: FilePath -> Int -> IO ()
runWSAgentsMode checkpoint humanN = do
  let (gs, gr) = noMerci 3
      players      = S.toList (gs ^. #players)
      script       = "python/ws_agent.py"
      humanPlayer  = Player (toEnum humanN)
      aiPlayerNums = fromEnum . (\(Player p) -> p) <$> delete humanPlayer players
  interface <- buildInterface players
  let channels = fmap (\(PlayerInterface fc tc) -> (fc, tc)) (interface ^. #playerInterfaces)
  gameToBrickBChan <- newBChan 100
  brickToGameBChan <- newBChan 100
  let playerAgent = brickAgent (fst $ channels M.! humanPlayer) gameToBrickBChan
                                (snd $ channels M.! humanPlayer) brickToGameBChan
      gsv     = viewGameStateAs' gs humanPlayer
      initTUI = TUIState gsv humanPlayer ShowState [] brickToGameBChan Nothing True []
  forM_ aiPlayerNums $ \n ->
    void $ forkIO $ void $ spawnAgileRLAgent script checkpoint (toEnum n)
  let gameLoop = do
        let (gs', gr') = noMerci 3
        void $ runGameSeparateChannels "nomerci.log" interface gs' gr'
        gameLoop
  withWorker (runAgentIO playerAgent)
    $ withWorker gameLoop
    $ withWorker (server (length aiPlayerNums) gs interface)
    $ void (customMainWithDefaultVty (Just gameToBrickBChan) app initTUI)
