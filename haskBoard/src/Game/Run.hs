module Game.Run where

import Control.Concurrent (Chan)
import Control.Lens (to, (^.))
import Data.Finitary (Finitary)
import qualified Data.Set as S
import Effectful (runEff)
import Effectful.Crypto.RNG
import Effectful.Reader.Static (runReader)
import Effectful.State.Static.Shared (evalState)
import Game.Choose
import Game.Controller (GameController, PlayerInterface, chooseInterface, commonInterface)
import Game.GameE
import Game.GameState
import Game.Player (Player)
import Interface.Choose (GameToInterfacePayload)
import Log
import System.IO (IOMode (..), withFile)

runGameCommonChannels ::
  ( Ord l,
    Ord r,
    Ord cn,
    Enum cn,
    Bounded cn,
    Show ph,
    Show cn,
    Show l,
    Show r,
    Show pl,
    Show i,
    Eq ph,
    Finitary cn,
    Finitary l
  ) =>
  Player ->
  GameState l cn r ph pl i ->
  GameRules l cn r ph pl i ->
  Chan (GameToInterfacePayload l cn r ph pl i) ->
  Chan pl ->
  IO (GameState l cn r ph pl i, [Player])
runGameCommonChannels p gameState gameRules chanGameToClient chanClientToGame = do
  gen <- newCryptoRNGState
  withFile "log" WriteMode $ \handle ->
    runEff
      . evalState gameState
      . runCryptoRNG gen
      . runReader gameRules
      -- . chooseChan (LookAs p) chanGameToClient chanClientToGame
      . chooseInterface (commonInterface thePlayers chanGameToClient chanClientToGame)
      . logToFile DebugLevel handle
      $ playGameTurns (gameRules ^. #setupPhase)
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
    Show i,
    Eq ph,
    Finitary cn,
    Finitary l
  ) =>
  GameState l cn r ph pl i ->
  GameRules l cn r ph pl i ->
  GameController l cn r ph pl i ->
  IO (GameState l cn r ph pl i, [Player])
runGameFromInterfaces gameState gameRules controller = do
  gen <- newCryptoRNGState
  withFile "log" WriteMode $ \handle ->
    runEff
      . evalState gameState
      . runCryptoRNG gen
      . runReader gameRules
      -- . chooseChan (LookAs p) chanGameToClient chanClientToGame
      . chooseInterface controller
      . logToFile DebugLevel handle
      $ playGameTurns (gameRules ^. #setupPhase)
