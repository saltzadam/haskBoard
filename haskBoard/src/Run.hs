module Run (runGameSeparateChannels, runGameSeparateChannelsNoLogs) where

import Control.Lens ((^.))
import Game.Constraints (GameCounter, GameLocation, GamePhase, GamePlay, GameResource)
import Effectful (runEff)
import Effectful.Crypto.RNG
import Effectful.Reader.Static (runReader)
import Effectful.State.Static.Shared (evalState)
import Game.GameE
import Game.GameState
import Game.Player (Player)
import Interface.Controller (GameController, chooseInterface)
import Log
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (IOMode (..), withFile)
import Data.Aeson (ToJSONKey)

runGameSeparateChannels ::
  (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl, ToJSONKey r) =>
  FilePath -> -- old log
  FilePath -> -- json log
  GameController l cn r ph pl ->
  GameState l cn r ph pl ->
  GameRules l cn r ph pl ->
  IO (GameState l cn r ph pl, [Player])
runGameSeparateChannels logFile jsonFile controller gameState gameRules = do
  gen <- newCryptoRNGState
  createDirectoryIfMissing True (takeDirectory jsonFile)
  withFile logFile WriteMode
    (\handle -> withFile jsonFile AppendMode
      ( \handle' ->
          runEff
            . evalState gameState
            . runCryptoRNG gen
            . runReader gameRules
            . chooseInterface controller
            . logToFile DebugLevel handle
            . logToFileJSON GameLevel handle'
            $ playGameTurns (gameRules ^. #setupPhase)
      )
    )

runGameSeparateChannelsNoLogs ::  (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl, ToJSONKey r)=> GameController l cn r ph pl -> GameState l cn r ph pl -> GameRules l cn r ph pl -> IO (GameState l cn r ph pl, [Player])
runGameSeparateChannelsNoLogs controller gameState gameRules = do
   gen <- newCryptoRNGState
   runEff
    . evalState gameState
    . runCryptoRNG gen
    . runReader gameRules
    . chooseInterface controller
    . nullLoggerHandleText DebugLevel
    . nullLoggerHandle GameLevel
    $ playGameTurns (gameRules ^. #setupPhase)

 
