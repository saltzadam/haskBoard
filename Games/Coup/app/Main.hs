-- | Local Coup: you drive Player 1 through the Brick TUI; the remaining seats
-- are filled by random-move AI agents. No trained checkpoint required.
module Main where

import Brick (customMainWithDefaultVty)
import Brick.BChan (newBChan)
import Brick.Game.Tui (TUIMode (..), TUIState (..))
import Control.Concurrent (forkIO)
import Control.Lens ((^.))
import Control.Monad (forM_, void, when)
import Coup (coup, coupRules, initGameState)
import Data.List (elemIndex)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Game.Player (displayPlayer, mkPlayers)
import Game.View (viewGameStateAs')
import Interface.Agent (brickAgent, randomAgent, runAgentIO)
import Interface.Controller (PlayerInterface (..), buildInterface)
import Interface.Protocol (RewardConfig (ZeroSum))
import Run (runGameSeparateChannelsNoLogs)
import Run.Game (RunMode (..), runGame)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)
import Tui (app)

main :: IO ()
main = do
  args <- getArgs
  let argPairs = zip args (drop 1 args)
      numPlayers = fromMaybe 3 $ lookup "--players" argPairs >>= readMaybe
  when (numPlayers < 2 || numPlayers > 6) $ do
    putStrLn "Error: --players must be between 2 and 6"
    exitFailure
  let -- Training / trained-play modes go through the shared runGame harness.
      run = runGame coup (Just app) "logs/coup.log" "logs/coup.json" numPlayers
  case () of
    _
      | "--stdio" `elem` args -> run (Stdio ZeroSum) -- RL training (RLlib env)
      | "--collect" `elem` args -> run Collect -- BC data collection
      | "--ws-agents" `elem` args ->
          let checkpoint = args !! succ (fromJust (elemIndex "--ws-agents" args))
              humanN = fromMaybe 0 (lookup "--human-player" argPairs >>= readMaybe)
           in run (WSAgents checkpoint humanN) -- play vs a trained checkpoint
      | "--auto" `elem` args -> autoMain numPlayers -- headless random self-play
      | otherwise -> tuiMain numPlayers -- default: play vs random agents in the TUI

-- | Headless self-play: every seat is a random agent; run to completion and
-- print the winner. Verifies the turn loop, elimination and win condition.
autoMain :: Int -> IO ()
autoMain numPlayers = do
  let gs = initGameState numPlayers
      gr = coupRules
      players = mkPlayers numPlayers
  controller <- buildInterface players
  let pifs = controller ^. #playerInterfaces
  forM_ players $ \p -> do
    let PlayerInterface pFrom pTo = pifs M.! p
    void $ forkIO $ runAgentIO (randomAgent [] pFrom pTo)
  (_, winners) <- runGameSeparateChannelsNoLogs controller gs gr
  putStrLn ("winner(s): " ++ unwords (map displayPlayer winners))

tuiMain :: Int -> IO ()
tuiMain numPlayers = do
  let gs = initGameState numPlayers
      gr = coupRules
      players = mkPlayers numPlayers
      human = minBound -- Player PlayerOne
  controller <- buildInterface players
  let pifs = controller ^. #playerInterfaces

  gameToBrickBChan <- newBChan 100
  brickToGameBChan <- newBChan 100

  let PlayerInterface hFrom hTo = pifs M.! human
      playerAgent = brickAgent hFrom gameToBrickBChan hTo brickToGameBChan

  -- Random AI for every non-human seat.
  forM_ (filter (/= human) players) $ \p -> do
    let PlayerInterface pFrom pTo = pifs M.! p
    void $ forkIO $ runAgentIO (randomAgent [] pFrom pTo)
  void $ forkIO $ runAgentIO playerAgent

  -- Run games back-to-back off the main thread; when one ends, deal a fresh
  -- one (new shuffle via a fresh RNG). The TUI's end-game handler picks up the
  -- next game when the player presses Enter ("play again").
  let gameLoop = do
        _ <- runGameSeparateChannelsNoLogs controller (initGameState numPlayers) gr
        gameLoop
  void $ forkIO gameLoop

  let gsv = viewGameStateAs' gs human
      -- batchUpdates = False: redraw the board on every action so coins and
      -- influence update live, in step with the action log.
      initTUI = TUIState gsv human ShowState [] brickToGameBChan Nothing False []
  void $ customMainWithDefaultVty (Just gameToBrickBChan) app initTUI
