module Run where

import Control.Concurrent (Chan)
import Control.Lens (to, (^.))
import Game.Constraints (GameCounter, GameLocation, GamePhase, GamePlay, GameResource)
import qualified Data.Set as S
import Effectful (runEff)
import Effectful.Crypto.RNG
import Effectful.Reader.Static (runReader)
import Effectful.State.Static.Shared (evalState)
import Game.Choose
import Game.GameE
import Game.GameState
import Game.Player (Player)
import Interface.Controller (GameController, chooseInterface, commonInterface)
import Log
import System.IO (IOMode (..), withFile)

runGameCommonChannels ::
  (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl) =>
  FilePath ->
  GameState l cn r ph pl ->
  GameRules l cn r ph pl ->
  Chan (GameToInterfacePayload l cn r ph pl) ->
  Chan pl ->
  IO (GameState l cn r ph pl, [Player])
runGameCommonChannels logFile gameState gameRules chanGameToClient chanClientToGame = do
  gen <- newCryptoRNGState
  withFile logFile WriteMode
    ( \handle ->
        runEff
          . evalState gameState
          . runCryptoRNG gen
          . runReader gameRules
          -- . chooseChan (LookAs p) chanGameToClient chanClientToGame
          . chooseInterface (commonInterface thePlayers chanGameToClient chanClientToGame)
          . logToFile DebugLevel handle
          $ playGameTurns (gameRules ^. #setupPhase)
    )
  where
    thePlayers = gameState ^. #players . to S.toList

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
          -- . chooseChan (LookAs p) chanGameToClient chanClientToGame
          . chooseInterface controller
          . logToFile DebugLevel handle
          $ playGameTurns (gameRules ^. #setupPhase)
    )

-- TODO: remove bounded, enum
runGameFromInterfaces ::
  (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl) =>
  FilePath ->
  GameState l cn r ph pl ->
  GameRules l cn r ph pl ->
  GameController l cn r ph pl ->
  IO (GameState l cn r ph pl, [Player])
runGameFromInterfaces logFile gameState gameRules controller = do
  gen <- newCryptoRNGState
  withFile logFile WriteMode $ \handle ->
    runEff
      . evalState gameState
      . runCryptoRNG gen
      . runReader gameRules
      -- . chooseChan (LookAs p) chanGameToClient chanClientToGame
      . chooseInterface controller
      . logToFile DebugLevel handle
      $ playGameTurns (gameRules ^. #setupPhase)
