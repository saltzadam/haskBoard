{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Game.GameE (playGameTurns) where

import Control.Applicative (asum, liftA2)
import Control.Lens (to, (^.))
import Control.Monad (void)
import Control.Monad.Free
import Data.Finitary (Finitary)
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Effectful
import Effectful.Crypto.RNG (CryptoRNG (..), RNG (..))
import FinitaryMap (ftAt)
import Game.Choose
import Game.GameAction
import Game.GameState
import Game.Location (LocationShape (..), decrement, increment, inventory, setCounter, swap, transfer, transferCounter)
import Game.Player (Player, Turn (..))
import Game.Rules
import Game.Visibility (makeInvisible, makeVisible)
import Log
import ShuffleRNG

-- TODO: some kind of history besides log
-- TODO: consider modifying/assign w/ built-in updateGS

updateGS :: (GameInteract l cn r ph pl :> es, Interface l cn r ph pl :> es) => Eff es ()
updateGS = getGameState >>= update

logAction2 :: (GameInteract l cn r ph pl :> es, Log2 :> es, Show cn, Show ph, Ord r, Eq l, Show r, Show l, Eq cn) => GameAction l cn r ph -> Eff es ()
logAction2 (IncrementCounter cn) = do
  val <- useGameState (counter cn . #val)
  logComponent (T.pack $ "Incremented " ++ show cn ++ " to " ++ show val)
logAction2 (DecrementCounter cn) = do
  val <- useGameState (counter cn . #val)
  logComponent (T.pack $ "Decremented " ++ show cn ++ " to " ++ show val)
logAction2 (SetCounter cn i) = logComponent (T.pack $ "Set " ++ show cn ++ " to " ++ show i)
logAction2 (RollCounter cn) = do
  val <- useGameState (counter cn . #val)
  logComponent (T.pack $ "Rolled " ++ show cn ++ " to " ++ show val)
logAction2 (TransferCounter cn cn') = logComponent (T.pack $ "Moved one from " ++ show cn ++ " to " ++ show cn')
logAction2 (Shuffle l) = logComponent (T.pack $ "Shuffled " ++ show l)
logAction2 (MakeVisibleTo l p) = logComponent (T.pack $ "Made " ++ show l ++ "visible to " ++ show p)
logAction2 (MakeInvisibleTo l p) = logComponent (T.pack $ "Made " ++ show l ++ "invisible to " ++ show p)
logAction2 EndPhase = logComponent (T.pack "Ended phase")
logAction2 DoNothing = pure ()
logAction2 AdvanceTurn = logComponent "advanced turn"
logAction2 (SetNextTurn turn) = logComponent (T.pack $ "set next turn: " ++ show turn)
logAction2 (EndGame winners) = logComponent (T.pack ("Game over! Winners: " ++ show winners))
logAction2 (MkTransfer l l' r) = do
  invl <- show . inventory <$> useGameState (location l)
  invl' <- show . inventory <$> useGameState (location l')
  logComponent
    (T.pack $ "Transfered " ++ show r ++ " from " ++ show l ++ " to " ++ show l' ++ "\n Contents of " ++ show l ++ ": " ++ invl ++ "\n Contents of " ++ show l' ++ ": " ++ invl')
logAction2 (MkSwap l l' r r') = do
  invl <- show . inventory <$> useGameState (location l)
  invl' <- show . inventory <$> useGameState (location l')
  logComponent
    (T.pack $ "Swapped " ++ show r ++ " and " ++ show r' ++ " between " ++ show l ++ " and " ++ show l' ++ "\n Contents of " ++ show l ++ ": " ++ invl ++ "\n Contents of " ++ show l' ++ ": " ++ invl')
logAction2 (MakeAnnouncement speaker announcement) =
  let speaker' = maybe "Nobody" show speaker
   in logComponent (T.pack (speaker' ++ " announced: ") <> announcement)

-- order:
-- modify
-- update
-- log
-- continue (or other control)
-- TODO: code repetition!!!
logAndContinue :: (Log2 :> es, GameInteract l cn r ph pl :> es, Interface l cn r ph pl :> es, Ord r, Eq l, Show cn, Show ph, Show r, Show l, Eq cn) => GameAction l cn r ph -> Eff es PhaseControl
logAndContinue a = do
  updateGS
  logAction2 a
  continueGame

runGameAction :: forall l r cn ph pl es. (Ord l, Ord r, RNG :> es, GameInteract l cn r ph pl :> es, Eq cn, Show ph, Show cn, Show l, Show r, Log2 :> es, Eq ph, Interface l cn r ph pl :> es) => GameAction l cn r ph -> Eff es PhaseControl
runGameAction DoNothing = continueGame
runGameAction a@(MkTransfer l l' r) = do
  modifyingGameState (#objects . #locations) (transfer r l l')
  logAndContinue a
runGameAction a@(MkSwap l l' r r') = do
  modifyingGameState (#objects . #locations) (swap r r' l l')
  logAndContinue a
runGameAction a@(IncrementCounter c) = do
  modifyingGameState (counter c) increment
  logAndContinue a
runGameAction a@(DecrementCounter c) = do
  modifyingGameState (counter c) decrement
  logAndContinue a
runGameAction a@(SetCounter c v) = do
  modifyingGameState (counter c) (`setCounter` v)
  logAndContinue a
runGameAction a@(RollCounter c) = do
  (bl, bu) <- useGameState (counter c . #bounds)
  newVal <- randomR (bl, bu)
  assignGameState (counterVal c) newVal
  logAndContinue a
runGameAction a@(TransferCounter cnfrom cnto) = do
  modifyingGameState (#objects . #counters) (transferCounter cnfrom cnto)
  logAndContinue a
runGameAction a@(Shuffle l) = do
  loc <- useGameState (#objects . #locations . ftAt l)
  case loc of
    Deck cards -> do
      -- "The sequence (r1,...r[n-1]) of numbers such that r[i] is an
      -- independent sample from a uniform random distribution
      -- [0..n-i]"
      -- let makeSample_r i = randomR (0,length cards - i)
      -- sample <- traverse makeSample_r  [1..(length cards - 1)]
      shuffled <- (Seq.fromList) <$> shuffleRNG (F.toList cards)
      assignGameState (#objects . #locations . ftAt l) (Deck shuffled)
    _ -> pure ()
  logAndContinue a
runGameAction a@(MakeVisibleTo p lc) = do
  modifyVisibility (\vis -> makeVisible vis p lc)
  logAndContinue a
runGameAction a@(MakeInvisibleTo p lc) = do
  modifyVisibility (\vis -> makeInvisible vis p lc)
  logAndContinue a
runGameAction a@EndPhase = do
  logAction2 a
  return PCEndPhase
runGameAction a@AdvanceTurn = do
  logAction2 a
  return PCEndTurn
runGameAction a@(SetNextTurn turn) = do
  assignGameState #nextTurn turn
  logAndContinue a
runGameAction a@(EndGame winners) = do
  announceWinners winners
  logAction2 a
  return (PCEndGame winners)
runGameAction a@(MakeAnnouncement speaker announcement) = do
  announce speaker announcement
  logAndContinue a

-- runPhaseNodes' :: forall l r cn ph pl es i a. (Ord l, Ord r, Ord cn, Finitary cn, Interface l cn r ph pl :> es, GameInteract l cn r ph pl :> es, RNG :> es, Show ph, Show cn, Show l, Show r, Show pl, Log2 :> es, Eq ph, GameRun l cn r ph pl :> es) => [GameRule l cn r ph pl a] -> Eff es PhaseControl
-- runPhaseNodes' rules = fromMaybe PCContinue <$> foldM go Nothing rules
--   where
--     -- run until you reach a PhaseControl besides PCContinue
--     go :: Maybe PhaseControl -> GameRule l cn r ph pl a1 -> Eff es (Maybe PhaseControl)
--     go acc rule = do
--       result <- runRuleControl rule
--       if result == PCContinue
--         then return acc
--         else return (Just result)

continueGame :: Eff es PhaseControl
continueGame = return PCContinue

-- all control stuff

runFromPhases :: forall l cn r ph pl es. (GameInteract l cn r ph pl :> es, Ord l, Finitary cn, Show ph, Interface l cn r ph pl :> es, RNG :> es, Log2 :> es, Ord r, Eq ph, Show cn, Ord cn, Show l, Show r, Show pl, GameRun l cn r ph pl :> es) => [ph] -> Eff es TurnControl
runFromPhases phases = fromMaybe TEndTurn . asum <$> traverse handlePhase phases
  where
    handlePhase :: ph -> Eff es (Maybe TurnControl)
    handlePhase phase = do
      assignGameState #currentPhase phase
      Phase _ newNodes <- getPhases <*> pure phase
      result <- runRuleControl newNodes
      return $ case result of
        PCEndTurn -> Just TEndTurn
        PCEndGame winners -> Just (TEndGame winners)
        PCEndPhase -> Nothing
        PCContinue -> Nothing

playGameTurns :: forall l cn r ph pl es. (Ord l, Ord r, Ord cn, Finitary cn, RNG :> es, Interface l cn r ph pl :> es, GameInteract l cn r ph pl :> es, Show ph, Show cn, Show l, Show r, Show pl, Log2 :> es, Eq ph, GameRun l cn r ph pl :> es) => Maybe ph -> Eff es (GameState l cn r ph pl, [Player])
playGameTurns setupPhaseName = do
  phases <- getPhases
  case phases <$> setupPhaseName of
    Just (Phase _ nodes) -> void . runRuleControl $ nodes
    Nothing -> return ()
  winners <- playGameTurns'
  liftA2 (,) getGameState (pure winners)
  where
    playGameTurns' = do
      gs <- getGameState
      result <- runFromPhases (gs ^. #currentTurn . #turnPhases . to NE.toList)
      case result of
        TEndGame winners -> return winners
        TEndTurn -> do
          nextTurn <- useGameState #nextTurn
          case nextTurn of
            Nothing -> error "no next turn" -- TODO: make proper exception
            Just t -> do
              logGame "end of turn"
              assignGameState #currentTurn t
              assignGameState #nextTurn Nothing
              updateGS
              playGameTurns'

-- Run rule and return appropriate PhaseControl
runRuleControl' :: forall l r cn ph pl es i a. (Ord l, Ord r, Ord cn, Finitary cn, Interface l cn r ph pl :> es, GameInteract l cn r ph pl :> es, RNG :> es, Show ph, Show cn, Show l, Show r, Show pl, Log2 :> es, Eq ph, GameRun l cn r ph pl :> es) => Free (GameRuleF l cn r ph pl) a -> Eff es PhaseControl
runRuleControl' (Free (Act action next)) = runGameAction action >> runRuleControl' next
runRuleControl' (Free (MakeChoice opts _)) = do
  gs <- getGameState
  pl <- choose gs opts
  runner <- getRunner
  let GameRule run = runner pl
  runRuleControl' run
runRuleControl' (Free (LookLocation l next)) = do
  shape <- useGameState (#objects . #locations . ftAt l)
  runRuleControl' (next shape)
runRuleControl' (Free (LookCounter cn next)) = do
  counter <- useGameState (#objects . #counters . ftAt cn)
  runRuleControl' (next counter)
runRuleControl' (Free (LookCurrentPhase next)) = useGameState #currentPhase >>= runRuleControl' . next
runRuleControl' (Free (LookCurrentTurnOwner next)) = useGameState (#currentTurn . to (\(Turn p _) -> p)) >>= runRuleControl' . next
runRuleControl' (Free (LookPlayers next)) = useGameState #players >>= runRuleControl' . next
runRuleControl' (Pure _) = return PCContinue

runRuleControl :: (Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l, Show r, Show pl, Interface l cn r ph pl :> es, GameInteract l cn r ph pl :> es, GameRun l cn r ph pl :> es, RNG :> es, Log2 :> es, Eq ph) => GameRule l cn r ph pl a -> Eff es PhaseControl
runRuleControl (GameRule rule) = runRuleControl' rule
