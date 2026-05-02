module Run where

import Control.Concurrent (Chan)
import Control.Lens (to, (^.))
import Data.Finitary (Finitary)
import qualified Data.Set as S
import Effectful (runEff)
import Effectful.Crypto.RNG
import Effectful.Reader.Static (runReader)
import Effectful.State.Static.Shared (evalState)
import Game.Choose
import Game.GameE
import Game.GameState
import Game.Player (Player)
import Interface.Controller (GameController, buildInterface, chooseInterface, commonInterface)
import Log
import System.IO (IOMode (..), withFile)

runGameCommonChannels ::
  (Ord l, Ord r, Ord cn, Show ph, Show cn, Show l, Show r, Show pl, Eq ph, Finitary cn, Finitary l) =>
  FilePath ->
  Player ->
  GameState l cn r ph pl ->
  GameRules l cn r ph pl ->
  Chan (GameToInterfacePayload l cn r ph pl) ->
  Chan pl ->
  IO (GameState l cn r ph pl, [Player])
runGameCommonChannels logFile p gameState gameRules chanGameToClient chanClientToGame = do
  gen <- newCryptoRNGState
  withFile logFile WriteMode $
    ( \handle ->
        runEff
          . evalState gameState
          . runCryptoRNG gen
          . runReader gameRules
          -- . chooseChan (LookAs p) chanGameToClient chanClientToGame
          . chooseInterface (commonInterface thePlayers chanGameToClient chanClientToGame)
          . logToFile DebugLevel handle
          $ (playGameTurns (gameRules ^. #setupPhase))
    )
  where
    thePlayers = gameState ^. #players . to S.toList

runGameSeparateChannels ::
  (Ord l, Ord r, Ord cn, Show ph, Show cn, Show l, Show r, Show pl, Eq ph, Finitary cn, Finitary l) =>
  FilePath ->
  GameController l cn r ph pl ->
  GameState l cn r ph pl ->
  GameRules l cn r ph pl ->
  IO (GameState l cn r ph pl, [Player])
runGameSeparateChannels logFile controller gameState gameRules = do
  gen <- newCryptoRNGState
  withFile logFile WriteMode $
    ( \handle ->
        runEff
          . evalState gameState
          . runCryptoRNG gen
          . runReader gameRules
          -- . chooseChan (LookAs p) chanGameToClient chanClientToGame
          . chooseInterface controller
          . logToFile DebugLevel handle
          $ (playGameTurns (gameRules ^. #setupPhase))
    )
   where
    thePlayers = gameState ^. #players . to S.toList

-- TODO: remove bounded, enum
runGameFromInterfaces ::
  ( Ord l,
    Ord r,
    Ord cn,
    Show ph,
    Show cn,
    Show l,
    Show r,
    Show pl,
    Eq ph,
    Finitary cn,
    Finitary l
  ) =>
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
