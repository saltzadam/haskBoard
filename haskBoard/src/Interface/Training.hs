module Interface.Training
  ( 
    stdioTrainingLoop,
    collectLoop
  )
where

import Control.Exception (IOException, catch)
import Control.Monad (void)
import Data.Aeson (decodeStrict)
import qualified Data.ByteString.Char8 as BS
import Game.Constraints (GameCounter, GameLocation, GamePhase, GamePlay, GameResource)
import Game.GameState (GameRules, GameState)
import Interface.Controller (GameController)
import Interface.Protocol (InMsg (..), RewardConfig)
import Run.Stdio (sendInit)
import Run (runGameSeparateChannels, runGameSeparateChannelsNoLogs)
import System.Exit (exitSuccess)
import System.IO (BufferMode (..), hSetBuffering, stdin, stdout)

data LoopMode = WaitForResetSignal | AutoReset

-- | Run the game in a loop for training or data collection.
-- Sends a single InitMsg on startup, then runs games forever.
-- In 'WaitForResetSignal' mode, waits for a {"type":"reset"} from stdin
-- between episodes. In 'AutoReset' mode, immediately starts the next game.
trainingLoop
  :: (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl)
  => LoopMode
  -> (GameState l cn r ph pl, GameRules l cn r ph pl)
  -> GameController l cn r ph pl
  -> RewardConfig
  -> IO ()
trainingLoop mode (gs0, gr0) controller rc = do
  hSetBuffering stdout LineBuffering
  case mode of WaitForResetSignal -> hSetBuffering stdin LineBuffering; _ -> pure ()
  sendInit gs0 gr0 rc
  betweenEpisodes
  loop
  where
    betweenEpisodes = case mode of
      WaitForResetSignal -> waitForReset
      AutoReset          -> pure ()
    loop = do
      void $ runGameSeparateChannelsNoLogs controller gs0 gr0
      betweenEpisodes
      loop

-- | Convenience wrapper: training loop that waits for reset signals from stdin.
stdioTrainingLoop
  :: (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl)
  => (GameState l cn r ph pl, GameRules l cn r ph pl)
  -> GameController l cn r ph pl
  -> RewardConfig
  -> IO ()
stdioTrainingLoop = trainingLoop WaitForResetSignal

-- | Convenience wrapper: training loop that auto-resets between episodes.
collectLoop
  :: (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl)
  => (GameState l cn r ph pl, GameRules l cn r ph pl)
  -> GameController l cn r ph pl
  -> RewardConfig
  -> IO ()
collectLoop = trainingLoop AutoReset

waitForReset :: IO ()
waitForReset =
  (do
    line <- BS.getLine
    case decodeStrict line :: Maybe InMsg of
      Just ResetMsg -> return ()
      _             -> waitForReset)
  `catch` (\(_ :: IOException) -> exitSuccess)
