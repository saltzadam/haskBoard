module Run (runGameSeparateChannels) where

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
import System.IO (IOMode (..), withFile)

runGameSeparateChannels ::
  (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl) =>
  FilePath ->
  GameController l cn r ph pl ->
  GameState l cn r ph pl ->
  GameRules l cn r ph pl ->
  IO (GameState l cn r ph pl, [Player])
runGameSeparateChannels logFile controller gameState gameRules = do
  gen <- newCryptoRNGState
  withFile logFile WriteMode
    ( \handle ->
        runEff
          . evalState gameState
          . runCryptoRNG gen
          . runReader gameRules
          . chooseInterface controller
          . logToFile DebugLevel handle
          $ playGameTurns (gameRules ^. #setupPhase)
    )
