{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use uncurry" #-}

module Main where

import Brick (customMainWithDefaultVty)
import Brick.BChan (newBChan)
import Brick.Game.Tui (TUIMode (..), TUIState (..))
import Control.Concurrent (forkIO, newMVar)
import Control.Concurrent.Async (withAsync)
import Control.Lens ((^.))
import Control.Monad (forM_, unless, void)
import Data.List (delete, elemIndex)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Game.Player (Player (..))
import Game.View (viewGameStateAs')
import Interface.Agent (brickAgent, randomAgent, runAgentIO)
import Interface.Controller (PlayerInterface (..), buildInterface)
import Game.Location (inventoryTotals)
import Interface.Server (server, spawnRLLibAgent)
import Interface.Stdio (runStdioAgent)
import Interface.Training (stdioTrainingLoop, collectLoop)
import NoMerci
import Run (runGameSeparateChannels)
import System.Directory (doesDirectoryExist, doesFileExist, makeAbsolute)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import Tui (app)

withWorker :: IO a -> IO a -> IO a
withWorker outer inner = withAsync outer $ const inner

main :: IO ()
main = do
  args <- getArgs
  case () of
    _ | "--stdio"     `elem` args -> runGame Stdio
      | "--collect"   `elem` args -> runGame Collect
      | "--tui"       `elem` args -> runGame TUI
      | "--ws-agents" `elem` args ->
          let checkpoint = args !! succ (fromJust (elemIndex "--ws-agents" args))
              humanN     = maybe 0 read (lookup "--human-player" (zip args (drop 1 args)))
          in runGame (WSAgents checkpoint humanN)
      | otherwise -> runGame Server

data RunMode = Server | Stdio | Collect | WSAgents FilePath Int

runGame :: RunMode -> IO ()
runGame mode = do
  let (gs,gr,hints) = noMerci 3
  let players = S.toList (gs ^. #players)
  let totals = inventoryTotals (gs ^. #objects ^. #locations)
  interface <- buildInterface players
  case mode of
    Server -> withWorker
      (void $ runGameSeparateChannels "nomerci.log" interface gs gr)
      (server totals 3 gs gr interface Nothing)
    Stdio -> do 
      lock <- newMVar ()
      forM_ (M.toList (interface ^. #playerInterfaces)) $ \(p, PlayerInterface fromChan toChan) ->
        void $ forkIO $ runStdioAgent totals [] False p lock players gr fromChan toChan
      stdioTrainingLoop (gs, gr) "training.log" interface
    Collect -> do
      lock <- newMVar ()
      forM_ (M.toList (interface ^. #playerInterfaces)) $ \(p, PlayerInterface fromChan toChan) ->
        void $ forkIO $ runStdioAgent totals hints True p lock players gr fromChan toChan
      collectLoop (gs, gr) "collect.log" interface
    WSAgents checkpointPath humanN -> do
      let humanPlayer  = Player (toEnum humanN)

      absCheckpoint <- makeAbsolute checkpointPath 
      -- Support both algo checkpoint layout (learner_group/learner/rl_module/)
      -- and flat BC checkpoint layout (player_0/module_state.pkl)
      let rlModuleDir = absCheckpoint </> "learner_group" </> "learner" </> "rl_module"
          bcDir       = absCheckpoint </> "player_0"
      algoExists <- doesDirectoryExist rlModuleDir
      bcExists   <- doesDirectoryExist bcDir
      unless (algoExists || bcExists) $ do
        putStrLn $ "Error: No checkpoint found in: " ++ absCheckpoint
        putStrLn $ "Expected either learner_group/learner/rl_module/ (algo) or player_0/ (BC)"
        exitFailure
      -- Find the script whether CWD is the project root or python/
      scriptInCwd <- doesFileExist "ws_agent_rllib.py"
      scriptInPyCwd <- doesFileExist "python/ws_agent_rllib.py"
      unless (scriptInCwd || scriptInPyCwd) $ do
        putStrLn $ "Error: No script found "
        exitFailure
      let script = if scriptInCwd then "ws_agent_rllib.py" else "python/ws_agent_rllib.py"
          aiPlayerNums = fromEnum . (\(Player p) -> p) <$> delete humanPlayer players
      let channels = fmap (\(PlayerInterface fc tc) -> (fc, tc)) (interface ^. #playerInterfaces)
      gameToBrickBChan <- newBChan 100
      brickToGameBChan <- newBChan 100
      let playerAgent = brickAgent (fst $ channels M.! humanPlayer) gameToBrickBChan
                                    (snd $ channels M.! humanPlayer) brickToGameBChan
          gsv     = viewGameStateAs' gs humanPlayer
          initTUI = TUIState gsv humanPlayer ShowState [] brickToGameBChan Nothing True []
      forM_ aiPlayerNums $ \n ->
        void $ forkIO $ void $ spawnRLLibAgent script absCheckpoint (toEnum n)
      let gameLoop = do
            let (gs', gr',hints) = noMerci 3
            void $ runGameSeparateChannels "nomerci.log" interface gs' gr'
            gameLoop
      withWorker (runAgentIO playerAgent)
        $ withWorker gameLoop
        $ withWorker (server totals (length aiPlayerNums) gs gr interface Nothing)
        $ void (customMainWithDefaultVty (Just gameToBrickBChan) app initTUI)
