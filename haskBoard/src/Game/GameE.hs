{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Game.GameE (playGameTurns) where

import Control.Lens (to, (^.), view)
import Control.Monad (join, replicateM, void)
import Data.Bitraversable
import Data.Finitary (Finitary)
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Effectful
import Effectful.Crypto.RNG
  ( CryptoRNG (..),
    RNG (..),
  )
import FinitaryMap (ftAt)
import GHC.Generics (Generic)
import Game.Choose
import Game.GameNode (GameAction (..), GameNode)
import Game.GameState
import Game.Location (LocationShape (..), decrement, increment, inventory, setCounter, transfer, swap, transferCounter)
import Game.Options
import Game.Visibility (makeInvisible, makeVisible)
import Log
import Game.Player (Player)
import Util (shuffleRNG)
-- TODO: (fun) some kind of history besides log

updateGS :: (GameInteract l cn r ph pl i :> es, Interface l cn r ph pl i :> es) => Eff es ()
updateGS = getGameState >>= update

logAction2 :: (GameInteract l cn r ph pl i :> es, Log2 :> es, Show cn, Show ph, Show val, Ord r, Eq l, Show r, Show l) => GameAction l cn r ph -> val -> Eff es ()
logAction2 (IncrementCounter cn) val = logComponent (T.pack $ "Incremented " ++ show cn ++ " to " ++ show val)
logAction2 (DecrementCounter cn) val = logComponent (T.pack $ "Decremented " ++ show cn ++ " to " ++ show val)
logAction2 (SetCounter cn i) _ = logComponent (T.pack $ "Set " ++ show cn ++ " to " ++ show i)
logAction2 (RollCounter cn) val = logComponent (T.pack $ "Rolled " ++ show cn ++ " to " ++ show val)
logAction2 (AddCounter cn) _ = logComponent (T.pack $ "Added counter " ++ show cn)
logAction2 (RemoveCounter cn) _ = logComponent (T.pack $ "Removed counter " ++ show cn)
logAction2 (TransferCounter cn cn') _ = logComponent (T.pack $ "Moved one from " ++ show cn ++ " to " ++ show cn')
logAction2 (Shuffle l) _ = logComponent (T.pack $ "Shuffled " ++ show l)
-- logAction2 (ChangePhase ph) _ = logComponent (T.pack $ "Changed phase to " ++ show ph)
logAction2 (MakeVisibleTo l p) _ = logComponent (T.pack $ "Made " ++ show l ++ "visible to " ++ show p)
logAction2 (MakeInvisibleTo l p) _ = logComponent (T.pack $ "Made " ++ show l ++ "invisible to " ++ show p)
logAction2 EndPhase _ = logComponent (T.pack "Ended phase")
logAction2 DoNothing _ = pure ()
logAction2 AdvanceTurn _ = logComponent "Advanced turn"
logAction2 (EndGame winners) _ = logComponent (T.pack ("Game over! Winners: " ++ show winners))
logAction2 (MkTransfer l l' r) _ = do
  invl <- show . inventory <$> useGameState (location l)
  invl' <- show . inventory <$> useGameState (location l')
  logComponent
    ( T.pack $
        "Transfered "
          ++ show r
          ++ " from "
          ++ show l
          ++ " to "
          ++ show l'
          ++ "\n Contents of "
          ++ show l
          ++ ": "
          ++ invl
          ++ "\n Contents of "
          ++ show l'
          ++ ": "
          ++ invl'
    )
logAction2 (MkSwap l l' r r') _ = do
  invl <- show . inventory <$> useGameState (location l)
  invl' <- show . inventory <$> useGameState (location l')
  logComponent
    ( T.pack $
        "Swapped "
          ++ show r
          ++ " and "
          ++ show r'
          ++ " between "
          ++ show l
          ++ " and "
          ++ show l'
          ++ "\n Contents of "
          ++ show l
          ++ ": "
          ++ invl
          ++ "\n Contents of "
          ++ show l'
          ++ ": "
          ++ invl'
    )

-- order:
-- modify
-- update
-- log
-- continue (or other control)
-- TODO: code repetition!!!
act :: forall l r cn ph pl i es. (Ord l, Ord r, RNG :> es, GameInteract l cn r ph pl i :> es, Eq cn, Show ph, Show cn, Show l, Show r, Log2 :> es, Eq ph, Interface l cn r ph pl i :> es) => GameAction l cn r ph -> Eff es PhaseControl
act DoNothing = continueGame
act a@(MkTransfer l l' r) =
  modifyingGameState (#objects . #locations) (transfer r l l')
    >> updateGS
    >> logAction2 a ' '
    >> continueGame
act a@(MkSwap l l' r r') =
    modifyingGameState (#objects . #locations) (swap r r' l l')
    >> updateGS
    >> logAction2 a ' '
    >> continueGame
act a@(IncrementCounter c) =
  modifyingGameState (counter c) increment
    >> updateGS
    >> (useGameState (counter c . #val) >>= logAction2 a)
    >> continueGame
act a@(DecrementCounter c) =
  modifyingGameState (counter c) decrement
    >> updateGS
    >> (useGameState (counter c . #val) >>= logAction2 a)
    >> continueGame
act a@(SetCounter c v) =
  modifyingGameState (counter c) (`setCounter` v)
    >> updateGS
    >> (useGameState (counter c . #val) >>= logAction2 a)
    >> continueGame
act a@(RollCounter c) = do
  (bl, bu) <- useGameState (counter c . #bounds)
  newVal <- randomR (bl, bu)
  assignGameState (counterVal c) (Just newVal)
  useGameState (counterVal c) >>= logAction2 a
  updateGS
  continueGame
act a@(AddCounter c) = do
    (bl, _) <- useGameState (counter c . #bounds)
    modifyingGameState (counter c) (`setCounter` bl)
    updateGS
    useGameState (counter c . #val) >>= logAction2 a
    continueGame
act a@(RemoveCounter c) = do
    assignGameState (counter c . #val) Nothing
    updateGS
    useGameState (counter c . #val) >>= logAction2 a
    continueGame
act a@(TransferCounter cnfrom cnto) = do
    modifyingGameState (#objects . #counters) (transferCounter cnfrom cnto)
    updateGS
    logAction2 a ' '
    continueGame
act a@(Shuffle l) = do
  loc <- useGameState (#objects . #locations . ftAt l)
  case loc of
    Deck cards -> do
      -- "The sequence (r1,...r[n-1]) of numbers such that r[i] is an
      -- independent sample from a uniform random distribution
      -- [0..n-i]"
      -- let makeSample_r i = randomR (0,length cards - i)
      -- sample <- traverse makeSample_r  [1..(length cards - 1)]
      shuffled <- Seq.fromList <$> shuffleRNG (F.toList cards)
      assignGameState (#objects . #locations . ftAt l) (Deck shuffled)
    _ -> pure ()
  logAction2 a ' '
  updateGS
  continueGame
act a@(MakeVisibleTo p lc) =
  modifyVisibility (\vis -> makeVisible vis p lc)
    >> logAction2 a ' '
    >> updateGS
    >> continueGame
act a@(MakeInvisibleTo p lc) =
  modifyVisibility (\vis -> makeInvisible vis p lc)
    >> logAction2 a ' '
    >> updateGS
    >> continueGame
act a@EndPhase = do
  logAction2 a ' '
  return PCEndPhase
act a@AdvanceTurn = do
  logAction2 a ' '
  return PCEndTurn
act a@(EndGame winners) = do
    announceWinners winners
    logAction2 a ' '
    return PCEndGame

chooseNode :: forall l cn r ph pl i es. (Interface l cn r ph pl i :> es, GameInteract l cn r ph pl i :> es, Show pl, Show i, Show l, Show r, Show cn, Show ph, Log2 :> es, GameRun l cn r ph pl i :> es) => Eff es (Options pl i) -> Eff es [Eff es [GameNode l cn r ph pl i]]
chooseNode cs = do
  options <- cs
  gs <- getGameState
  logGame (T.pack ("Choosing from " ++ displayOptions options))
  c <- choose gs options
  logGame (T.pack ("Chose " ++ show c))
  runner <- getRunner
  return (inject <$> runner c)

runNode :: forall l r cn ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, Interface l cn r ph pl i :> es, GameInteract l cn r ph pl i :> es, RNG :> es, Show ph, Show cn, Show l, Show r, Show pl, Show i, Log2 :> es, Eq ph, GameRun l cn r ph pl i :> es) => GameNode l cn r ph pl i -> Eff es (Either PhaseControl [Eff es [GameNode l cn r ph pl i]])
runNode aNode = bitraverse act (chooseNode . pure) (aNode ^. #node)

runPhaseNodes :: forall l r cn ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, GameInteract l cn r ph pl i :> es, Interface l cn r ph pl i :> es, RNG :> es, Show ph, Show cn, Show l, Show r, Show pl, Show i, Log2 :> es, Eq ph, GameRun l cn r ph pl i :> es) => [Eff es [GameNode l cn r ph pl i]] -> Eff es PhaseControl
runPhaseNodes [] = return PCEndPhase
runPhaseNodes (node : nodes) = do
  result <- unfoldNodes node
  downResult <- handleResult result
  case downResult of
    PCContinue -> runPhaseNodes nodes
    i -> return i
  where
    handleResult ::
      Either
        PhaseControl
        [Eff es [GameNode l cn r ph pl i]] ->
      Eff es PhaseControl
    handleResult result = case result of
      Left control -> return control
      Right nextLevelnodes ->
        if null nextLevelnodes
          then return PCContinue
          else runPhaseNodes nextLevelnodes

    unfoldNodes :: Eff es [GameNode l cn r ph pl i] -> (Eff es) (Either PhaseControl [Eff es [GameNode l cn r ph pl i]])
    unfoldNodes effNodes = do
      nodes <- effNodes
      fmap (fmap join . sequence) (traverse runNode nodes)

continueGame :: Eff es PhaseControl
continueGame = return PCContinue

-- all control stuff

data PhaseControl = PCContinue | PCEndPhase | PCEndTurn | PCEndGame deriving (Eq, Ord, Show, Generic)

-- data GameControl ph = CutoffPhase | CutoffTurn | End deriving (Eq, Ord, Show, Generic)
data TurnControl = TEndTurn | TEndGame deriving (Eq, Ord, Show, Generic)

runFromPhases :: (GameInteract l cn r ph pl i :> es, Ord l, Finitary cn, Show ph, Interface l cn r ph pl i :> es, RNG :> es, Log2 :> es, Ord r, Eq ph, Show cn, Ord cn, Show l, Show r, Show pl, Show i, GameRun l cn r ph pl i :> es) => [ph] -> Eff es TurnControl
runFromPhases (phase : theRest) = do
  assignGameState #currentPhase phase
  phases <- getPhases
  let newNodes = getPhaseNodes (phases phase)
  result <- runPhaseNodes newNodes
  case result of
    PCEndTurn -> return TEndTurn
    PCEndGame -> return TEndGame
    PCEndPhase -> runFromPhases theRest
    PCContinue -> runFromPhases theRest
runFromPhases [] = return TEndTurn

runTurns :: (GameInteract l cn r ph pl i :> es, Finitary cn, Interface l cn r ph pl i :> es, RNG :> es, Log2 :> es, Ord l, Ord r, Eq ph, Ord cn, Show ph, Show cn, Show l, Show r, Show pl, Show i, GameRun l cn r ph pl i :> es) => Turn ph -> Eff es TurnControl
runTurns turn = do
  let phases = NE.toList (turn ^. #turnPhases)
  runFromPhases phases

playGameTurns :: forall l cn r ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, RNG :> es, Interface l cn r ph pl i :> es, GameInteract l cn r ph pl i :> es, Show ph, Show cn, Show l, Show r, Show pl, Show i, Log2 :> es, Eq ph, GameRun l cn r ph pl i :> es) => Maybe ph -> Eff es (GameState l cn r ph pl i, [Player])
playGameTurns setupPhaseName = do
  phases <- getPhases
  maybe (return ()) (void . runPhaseNodes  . getPhaseNodes . phases) setupPhaseName
  void playGameTurns'
  (,) <$> getGameState <*> winner
  where
    playGameTurns' = do
      gs <- getGameState
      let currentTurn = gs ^. #currentTurn
      result <- runTurns currentTurn
      case result of
        TEndGame -> return TEndGame
        TEndTurn -> do
                    nextTurn <- useGameState #nextTurn <*> useGameState #currentTurn <*> useGameState #turns
                    assignGameState #currentTurn nextTurn
                    updateGS
                    playGameTurns'
