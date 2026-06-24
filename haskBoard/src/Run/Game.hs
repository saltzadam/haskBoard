module Run.Game
  (
   RunMode (..)
  , runGame
  ) where

import Brick (App, customMainWithDefaultVty)
import Brick.BChan (newBChan)
import Brick.Game.Tui (TUIMode (..), TUIState (..))
import Control.Concurrent (forkIO, newMVar)
import Control.Concurrent.Async (withAsync)
import Control.Lens ((^.))
import Control.Monad (forM_, unless, void)
import Data.List (delete)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Game.Agent (BEvent)
import Game.Constraints (GameCounter, GameLocation, GamePhase, GamePlay, GameResource)
import Game.GameState (GameRules, GameState)
import Game.Location (inventoryTotals)
import Game.Player (Player (..))
import Game.View (viewGameStateAs')
import Interface.Agent (brickAgent, runAgentIO)
import Interface.Controller (PlayerInterface (..), buildInterface)
import Interface.Hint (HintM)
import Run.Server (server, spawnRLLibAgent)
import Run.Stdio (runStdioAgent)
import Interface.Training (collectLoop, stdioTrainingLoop)
import Run (runGameSeparateChannels)
import System.Directory (doesDirectoryExist, doesFileExist, makeAbsolute)
import System.Exit (exitFailure)
import System.FilePath ((</>))

withWorker :: IO a -> IO a -> IO a
withWorker outer inner = withAsync outer $ const inner

data RunMode = Stdio | Collect | WSAgents FilePath Int

runGame
  :: (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl, Ord name)
  => (Int -> (GameState l cn r ph pl, GameRules l cn r ph pl, [HintM l cn r ph pl]))
  -> Maybe (App (TUIState l cn r ph pl) (BEvent l cn r ph pl) name)
  -> FilePath
  -> Int
  -> RunMode
  -> IO ()
runGame initGame mTuiApp logFile numPlayers mode = do
  let (gs,gr,hints) = initGame numPlayers
  let players = S.toList (gs ^. #players)
  let totals = inventoryTotals (gs ^. (#objects . #locations))
  interface <- buildInterface players
  case mode of
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
      let tuiApp = fromMaybe (error "WSAgents mode requires a TUI app") mTuiApp
          humanPlayer  = Player (toEnum humanN)

      absCheckpoint <- makeAbsolute checkpointPath
      let rlModuleDir = absCheckpoint </> "learner_group" </> "learner" </> "rl_module"
          bcDir       = absCheckpoint </> "player_0"
      algoExists <- doesDirectoryExist rlModuleDir
      bcExists   <- doesDirectoryExist bcDir
      unless (algoExists || bcExists) $ do
        putStrLn $ "Error: No checkpoint found in: " ++ absCheckpoint
        putStrLn "Expected either learner_group/learner/rl_module/ (algo) or player_0/ (BC)"
        exitFailure
      scriptInCwd <- doesFileExist "ws_agent_rllib.py"
      scriptInPyCwd <- doesFileExist "python/ws_agent_rllib.py"
      unless (scriptInCwd || scriptInPyCwd) $ do
        putStrLn "Error: No script found "
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
            let (gs', gr',_hints) = initGame numPlayers
            void $ runGameSeparateChannels logFile interface gs' gr'
            gameLoop
      withWorker (runAgentIO playerAgent)
        $ withWorker gameLoop
        $ withWorker (server totals (length aiPlayerNums) gs gr interface Nothing)
        $ void (customMainWithDefaultVty (Just gameToBrickBChan) tuiApp initTUI)
