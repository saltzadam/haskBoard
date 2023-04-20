{-# LANGUAGE DataKinds #-}
module Game.Run where
import Game.GameE
import Game.Visibility
import Effectful (Eff, IOE, runEff)
import Game.GameNode
import Data.Finitary
import Log
import Effectful.Crypto.RNG
import Game.GameState
import Game.Choose

runGameTurns ::(Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l,
 Show r, Show pl, Show i, Eq ph) => Game l cn r ph pl i
    -> IO (GameState l cn r ph pl i)
runGameTurns game = do
    gen <- newCryptoRNGState
    runEff 
      . runGameInteract (gameState game) (playRunner game) (visibility game) (setup game)
      . runCryptoRNG gen
      . chooseRandom
      . broadcastHandlerDummy
      . logStdOut DebugLevel
      $ playGameTurns
           

actionTurns :: (Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l,
 Show r, Show pl, Show i, Eq ph) =>
  GameState l cn r ph pl i
  -> PlayRunner l cn r ph pl i
  -> VisibilityMap l cn
  -> (GameState l cn r ph pl i -> [GameNode l cn r ph pl i])
  -> IO (GameState l cn r ph pl i)
actionTurns gdata playRunner vis setup = do
    gen <- newCryptoRNGState
    runEff . runGameInteract gdata playRunner vis setup . runCryptoRNG gen . broadcastHandlerDummy . chooseRandom . logStdOut DebugLevel $ playGameTurns
    
----- Condition, perhaps soon to be Observation?
-- type Condition l cn r ph pl i es a = Eff es a

runNodesAgainstState :: (Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l, Show r, Show pl, Show i, Eq ph) => GameState l cn r ph pl i -> PlayRunner l cn r ph pl i -> VisibilityMap l cn 
   -> (GameState l cn r ph pl i -> [GameNode l cn r ph pl i])
                     -> [GameNode l cn r ph pl i] -> IO (GameState l cn r ph pl i)
runNodesAgainstState game playRunner vis setup nodes = do
  gen <- newCryptoRNGState
  runEff . runGameInteract game playRunner vis setup . runCryptoRNG gen . broadcastHandlerDummy . chooseRandom . logStdOut DebugLevel $ playGivenNodes [pure nodes]

runEffNodesAgainstState ::
  (Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l, Show r, Show pl, Show i, Eq ph) =>
  GameState l cn r ph pl i ->
  PlayRunner l cn r ph pl i ->
  VisibilityMap l cn ->
  (GameState l cn r ph pl i -> [GameNode l cn r ph pl i]) ->
  [ Eff
      '[ Log2,
         Choosing,
         BroadcastState l cn r ph pl i,
         RNG,
         GameInteract 'Modify l cn r ph pl i,
         IOE
       ]
      [GameNode l cn r ph pl i]
  ] ->
  IO (GameState l cn r ph pl i)
runEffNodesAgainstState game playRunner vis setup nodes = do
  gen <- newCryptoRNGState
  runEff . runGameInteract game playRunner vis setup . runCryptoRNG gen . broadcastHandlerDummy . chooseRandom . logStdOut DebugLevel $ playGivenNodes nodes


