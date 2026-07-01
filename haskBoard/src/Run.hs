module Run (runGameSeparateChannels, runGameSeparateChannelsNoLogs) where

import Control.Lens ((^.))
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Effectful (runEff)
import Effectful.Crypto.RNG
import Effectful.Reader.Static (runReader)
import Effectful.State.Static.Shared (evalState)
import Game.Constraints (GameCounter, GameLocation, GamePhase, GamePlay, GameResource)
import Game.GameE
import Game.GameState
import Game.Player (Player (..))
import Interface.Controller (GameController, chooseInterface)
import Log
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (IOMode (..), withFile)

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
  gen <- newCryptoRNGState
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
        runEff
          . evalState gameState
          . runCryptoRNG gen
          . runReader gameRules
          . chooseInterface controller
          . runLogger writers
          $ playGameTurns (gameRules ^. #setupPhase)

runGameSeparateChannelsNoLogs ::
  (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl) =>
  GameController l cn r ph pl ->
  GameState l cn r ph pl ->
  GameRules l cn r ph pl ->
  IO (GameState l cn r ph pl, [Player])
runGameSeparateChannelsNoLogs controller gameState gameRules = do
  gen <- newCryptoRNGState
  runEff
    . evalState gameState
    . runCryptoRNG gen
    . runReader gameRules
    . chooseInterface controller
    . nullLogger
    $ playGameTurns (gameRules ^. #setupPhase)
