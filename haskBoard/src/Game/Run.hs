module Game.Run where
import Game.GameE
import Effectful (runEff)
import Game.GameNode
import Log
import Effectful.Crypto.RNG
import Game.GameState
import Game.Choose
import Data.Finitary (Finitary)
import Effectful.State.Static.Shared (evalState)
import Control.Concurrent (Chan)
import System.IO (withFile, IOMode (..))
import Effectful.Reader.Static (runReader)
import Game.Player (Player)
import Count (Cnt)
import Game.Controller (chooseInterface, commonInterface, PlayerInterface, GameController)
import Control.Lens ((^.), to)
import qualified Data.Set as S
import Data.Map (Map)

runGameCommonChannels :: (Ord l, Ord r, Ord cn, Enum cn, Bounded cn, Show ph, Show cn,
 Show l, Show r, Show pl, Show i, Eq ph, Finitary cn, Finitary l) =>
    Player
    -> GameState l cn r ph pl i
    -> (PlayRunner l cn r ph pl i, Int -> [GameNode l cn r ph pl i], ph -> Phase ph l cn r pl i, GameState l cn r ph pl i -> Player -> Cnt Int)
    -> Chan (GameToInterfacePayload l cn r ph pl i)
    -> Chan pl
    -> IO (GameState l cn r ph pl i, [Player])
runGameCommonChannels p gameState (playRunner, setup, phases, score) chanGameToClient chanClientToGame = do
    gen <- newCryptoRNGState
    withFile "log" WriteMode $ \handle -> 
      runEff
        . evalState gameState
        . runCryptoRNG gen
        . runReader (playRunner, setup, phases, score)
        -- . chooseChan (LookAs p) chanGameToClient chanClientToGame
        . chooseInterface (commonInterface thePlayers chanGameToClient chanClientToGame)
        . logToFile DebugLevel handle
        $ playGameTurns
    where
        thePlayers = gameState ^. #players . to S.toList

runGameFromInterfaces :: (Ord l, Ord r, Ord cn, Enum cn, Bounded cn, Show ph, Show cn,
 Show l, Show r, Show pl, Show i, Eq ph, Finitary cn, Finitary l) =>
    GameState l cn r ph pl i
    -> (PlayRunner l cn r ph pl i, Int -> [GameNode l cn r ph pl i], ph -> Phase ph l cn r pl i, GameState l cn r ph pl i -> Player -> Cnt Int)
    -> GameController l cn r ph pl i
    -> IO (GameState l cn r ph pl i, [Player])
runGameFromInterfaces gameState (playRunner, setup, phases, score) controller = do
    gen <- newCryptoRNGState
    withFile "log" WriteMode $ \handle -> 
      runEff
        . evalState gameState
        . runCryptoRNG gen
        . runReader (playRunner, setup, phases, score)
        -- . chooseChan (LookAs p) chanGameToClient chanClientToGame
        . chooseInterface controller
        . logToFile DebugLevel handle
        $ playGameTurns



