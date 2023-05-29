module Game.Run where
import Game.GameE
import Effectful (Eff, IOE, runEff, inject)
import Game.GameNode
import Log
import Effectful.Crypto.RNG
import Game.GameState
import Game.Choose
import Data.Finitary (Finitary)
import Effectful.State.Static.Shared (evalState)
import Control.Concurrent (Chan)
import Game.Options (Options)
import System.IO (withFile, IOMode (..))
import Effectful.Reader.Static (runReader)
import Game.View (GameStateView)
import Game.Player (Player)
import Game.Monad (ViewerType(..))


runGameChannels :: (Ord l, Ord r, Ord cn, Enum cn, Bounded cn, Show ph, Show cn,
 Show l, Show r, Show pl, Show i, Eq ph, Finitary cn) =>
    Player 
    -> GameState l cn r ph pl i
    -> (PlayRunner l cn r ph pl i, Int -> [GameNode l cn r ph pl i], ph -> Phase ph l cn r pl i)
    -> Chan
         (Either
            (GameStateView l cn r ph)
            (Options pl i))
    -> Chan pl
    -> IO (GameState l cn r ph pl i)
runGameChannels p gameState (playRunner, setup, phases) chanGameToClient chanClientToGame = do
    gen <- newCryptoRNGState
    withFile "log" WriteMode $ \handle -> 
      runEff
        . evalState gameState
        . runCryptoRNG gen
        . runReader (playRunner, setup, phases)
        . broadcastHandlerDummy
        . chooseChan (ViewAs p) chanGameToClient chanClientToGame
        . logToFile DebugLevel handle
        $ playGameTurns

-- runGameTurns ::(Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l,
--  Show r, Show pl, Show i, Eq ph) => Game l cn r ph pl i
--     -> IO (GameState l cn r ph pl i)
-- runGameTurns game = do
--     gen <- newCryptoRNGState
--     runEff 
--       . evalState game 
--       . runCryptoRNG gen
--       . broadcastHandlerDummy
--       . chooseRandom
--       . logStdOut DebugLevel
--       $ playGameTurns
           

-- actionTurns :: (Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l,
--  Show r, Show pl, Show i, Eq ph) =>
--   GameState l cn r ph pl i
--   -> PlayRunner l cn r ph pl i
--   -> (GameState l cn r ph pl i -> [GameNode l cn r ph pl i])
--   -> IO (GameState l cn r ph pl i)
-- actionTurns gdata playRunner setup = do
--     gen <- newCryptoRNGState
--     runEff . evalState (Game gdata playRunner setup) . runCryptoRNG gen . broadcastHandlerDummy . chooseRandom . logStdOut DebugLevel $ playGameTurns
    
-- ----- Condition, perhaps soon to be Observation?
-- -- type Condition l cn r ph pl i es a = Eff es a

-- runNodesAgainstState :: (Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l, Show r, Show pl, Show i, Eq ph) => GameState l cn r ph pl i -> PlayRunner l cn r ph pl i 
--    -> (GameState l cn r ph pl i -> [GameNode l cn r ph pl i])
--                      -> [GameNode l cn r ph pl i] -> IO (GameState l cn r ph pl i)
-- runNodesAgainstState game playRunner setup nodes = do
--   gen <- newCryptoRNGState
--   runEff . evalState (Game game playRunner setup) . runCryptoRNG gen . broadcastHandlerDummy . chooseRandom . logStdOut DebugLevel $ playGivenNodes [pure nodes]

-- runEffNodesAgainstState ::
--   (Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l, Show r, Show pl, Show i, Eq ph) =>
--   GameState l cn r ph pl i ->
--   PlayRunner l cn r ph pl i ->
--   (GameState l cn r ph pl i -> [GameNode l cn r ph pl i]) ->
--   [ Eff
--       '[ Log2,
--          Interface l cn r ph pl i,
--          BroadcastState l cn r ph pl i,
--          RNG,
--          GameInteract l cn r ph pl i,
--          IOE
--        ]
--       [GameNode l cn r ph pl i]
--   ] ->
--   IO (GameState l cn r ph pl i)
-- runEffNodesAgainstState game playRunner setup nodes = do
--   gen <- newCryptoRNGState
--   runEff . evalState (Game game playRunner setup) . runCryptoRNG gen . broadcastHandlerDummy . chooseRandom . logStdOut DebugLevel $ playGivenNodes nodes

