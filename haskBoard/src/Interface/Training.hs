module Interface.Training
  ( stdioTrainingLoop,
  )
where

import Control.Exception (IOException, catch)
import Control.Monad (void)
import Data.Aeson (decodeStrict)
import qualified Data.ByteString.Char8 as BS
import Game.Constraints (GameCounter, GameLocation, GamePhase, GamePlay, GameResource)
import Game.GameState (GameRules, GameState)
import Interface.Controller (GameController)
import Interface.Stdio (InMsg (..), sendInit)
import Run (runGameSeparateChannels)
import System.Exit (exitSuccess)
import System.IO (BufferMode (..), hSetBuffering, stdin, stdout)

-- | Run the game in a loop for MARL training.
-- Sends a single InitMsg on startup, then runs games forever.
-- After each game ends (SendWinners), waits for a {"type":"reset"} from stdin
-- before starting the next episode.  The GameController (and its channels)
-- is reused across episodes — build it once before calling this function.
stdioTrainingLoop
  :: (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl)
  => (GameState l cn r ph pl, GameRules l cn r ph pl)
  -> FilePath
  -> GameController l cn r ph pl
  -> IO ()
stdioTrainingLoop (gs0, gr0) logFile controller = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  sendInit gs0 gr0
  waitForReset  -- wait for Python's first reset() before starting
  loop
  where
    loop = do
      void $ runGameSeparateChannels logFile controller gs0 gr0
      waitForReset
      loop

waitForReset :: IO ()
waitForReset =
  (do
    line <- BS.getLine
    case decodeStrict line :: Maybe InMsg of
      Just ResetMsg -> return ()
      _             -> waitForReset)
  `catch` (\(_ :: IOException) -> exitSuccess)
