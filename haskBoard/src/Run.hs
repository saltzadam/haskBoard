module Run (runGameSeparateChannels, runGameSeparateChannelsNoLogs) where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Game.Constraints (GameCounter, GameLocation, GamePhase, GamePlay, GameResource)
import Game.GameE (Env (..), playGame)
import Game.GameState
import Game.Player (Player (..))
import Interface.Controller (GameController, controllerInterface)
import Log (LogTag (..), mkLogger, nullLogger)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (IOMode (..), withFile)
import System.Random (initStdGen)

runGameSeparateChannels ::
  (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl) =>
  FilePath -> -- action log
  FilePath -> -- json choice log
  FilePath -> -- winners csv
  Maybe Player -> -- human player
  GameController l cn r ph pl ->
  GameState l cn r ph pl ->
  GameRules l cn r ph pl ->
  IO (GameState l cn r ph pl, [Player])
runGameSeparateChannels logFile jsonFile winnersFile humanPlayer controller gameState gameRules = do
  gen <- initStdGen
  createDirectoryIfMissing True (takeDirectory jsonFile)
  withFile logFile WriteMode $ \hAction ->
    withFile jsonFile AppendMode $ \hChoice ->
      withFile winnersFile AppendMode $ \hWinners -> do
        let humanSuffix = maybe T.empty (\(Player p) -> T.pack ("," ++ show (fromEnum p))) humanPlayer
            writers = M.fromList
              [ (ActionLog,  TIO.hPutStrLn hAction)
              , (ChoiceLog,  const (return ()))
              , (WinnersLog, \t -> TIO.hPutStrLn hWinners (t <> humanSuffix))
              ]
        playGame
          Env {rules = gameRules, interface = controllerInterface controller, logger = mkLogger writers}
          gameState
          gen

runGameSeparateChannelsNoLogs ::
  (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl) =>
  GameController l cn r ph pl ->
  GameState l cn r ph pl ->
  GameRules l cn r ph pl ->
  IO (GameState l cn r ph pl, [Player])
runGameSeparateChannelsNoLogs controller gameState gameRules = do
  gen <- initStdGen
  playGame
    Env {rules = gameRules, interface = controllerInterface controller, logger = nullLogger}
    gameState
    gen
