{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Game.GameE (playGameTurns, evalRule, evalRule') where

import Control.Applicative (asum)
import Control.Lens (to, (^.))
import Control.Monad.Free
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Effectful
import Effectful.Crypto.RNG (CryptoRNG (..), RNG (..))
import FinitaryMap (ftAt)
import Game.Choose
import Game.Constraints (GameCounter, GameLocation, GamePhase, GamePlay, GameResource)
import Game.GameAction
import Game.GameState
import Game.Location (LocationShape (..), decrement, increment, inventory, setCounter, swap, transfer, transferCounter)
import Game.Player (Player (..), Turn (..))
import Game.Rules
import Game.Visibility (makeInvisible, makeVisible)
import Log
import ShuffleRNG

-- TODO: some kind of history besides log
-- TODO: consider modifying/assign w/ built-in updateGS

-- | Read-only interpreter for GameRule: evaluates Look* nodes against current
-- game state and returns the result. Must not be used with rules that contain
-- Act or MakeChoice nodes.
evalRule' :: (Eq l, Eq cn, GameInteract l cn r ph pl :> es) => Free (GameRuleF l cn r ph pl) a -> Eff es a
evalRule' (Pure a)                        = return a
evalRule' (Free (LookLocation l k))       = useGameState (#objects . #locations . ftAt l) >>= evalRule' . k
evalRule' (Free (LookCounter cn k))       = useGameState (#objects . #counters . ftAt cn) >>= evalRule' . k
evalRule' (Free (LookPlayers k))          = useGameState #players >>= evalRule' . k
evalRule' (Free (LookCurrentPhase k))     = useGameState #currentPhase >>= evalRule' . k
evalRule' (Free (LookCurrentTurnOwner k)) = useGameState (#currentTurn . to (\(Turn p _) -> p)) >>= evalRule' . k
evalRule' (Free (LookGameState k))        = getGameState >>= evalRule' . k
evalRule' (Free (Act _ _))                = error "evalRule: score function must not perform actions"
evalRule' (Free (MakeChoice _ _))         = error "evalRule: score function must not make choices"

evalRule :: (Eq l, Eq cn, GameInteract l cn r ph pl :> es) => GameRule l cn r ph pl a -> Eff es a
evalRule (GameRule rule) = evalRule' rule

updateGS :: (Eq l, Eq cn, GameInteract l cn r ph pl :> es, GameRun l cn r ph pl :> es, Interface l cn r ph pl :> es) => Eff es ()
updateGS = do
  gs      <- getGameState
  scoreFn <- getScore
  scores  <- M.fromList <$> traverse (\p -> fmap (\s -> (p, s)) (evalRule (scoreFn p))) (S.toList (gs ^. #players))
  update gs scores

tshow :: Show a => a -> T.Text
tshow = T.pack . show

logAction2 :: (GameInteract l cn r ph pl :> es, Log2 :> es, Show cn, Show ph, Ord r, Eq l, Show r, Show l, Eq cn) => GameAction l cn r ph -> Eff es ()
logAction2 (IncrementCounter cn) = do
  val <- useGameState (counter cn . #val)
  logComponent ("Incremented " <> tshow cn <> " to " <> tshow val)
logAction2 (DecrementCounter cn) = do
  val <- useGameState (counter cn . #val)
  logComponent ("Decremented " <> tshow cn <> " to " <> tshow val)
logAction2 (SetCounter cn i) = logComponent ("Set " <> tshow cn <> " to " <> tshow i)
logAction2 (RollCounter cn) = do
  val <- useGameState (counter cn . #val)
  logComponent ("Rolled " <> tshow cn <> " to " <> tshow val)
logAction2 (TransferCounter cn cn') = logComponent ("Moved one from " <> tshow cn <> " to " <> tshow cn')
logAction2 (Shuffle l) = logComponent ("Shuffled " <> tshow l)
logAction2 (MakeVisibleTo l p) = logComponent ("Made " <> tshow l <> " visible to " <> tshow p)
logAction2 (MakeInvisibleTo l p) = logComponent ("Made " <> tshow l <> " invisible to " <> tshow p)
logAction2 EndPhase = logComponent "Ended phase"
logAction2 DoNothing = pure ()
logAction2 (AdvanceTurn (Turn p _)) = logComponent ("advanced turn to " <> tshow p)
logAction2 (EndGame winners) = logComponent ("Game over! Winners: " <> tshow winners)
logAction2 (MkTransfer l l' r) = do
  invl <- tshow . inventory <$> useGameState (location l)
  invl' <- tshow . inventory <$> useGameState (location l')
  logComponent
    ("Transfered " <> tshow r <> " from " <> tshow l <> " to " <> tshow l' <> "\n Contents of " <> tshow l <> ": " <> invl <> "\n Contents of " <> tshow l' <> ": " <> invl')
logAction2 (MkSwap l l' r r') = do
  invl <- tshow . inventory <$> useGameState (location l)
  invl' <- tshow . inventory <$> useGameState (location l')
  logComponent
    ("Swapped " <> tshow r <> " and " <> tshow r' <> " between " <> tshow l <> " and " <> tshow l' <> "\n Contents of " <> tshow l <> ": " <> invl <> "\n Contents of " <> tshow l' <> ": " <> invl')
logAction2 (MakeAnnouncement speaker announcement) =
  let speaker' = maybe "Nobody" tshow speaker
   in logComponent (speaker' <> " announced: " <> announcement)

-- order:
-- modify
-- update
-- log
-- continue (or other control)
-- TODO: code repetition!!!
logAndContinue :: (Log2 :> es, GameInteract l cn r ph pl :> es, GameRun l cn r ph pl :> es, Interface l cn r ph pl :> es, Ord r, Eq l, Show cn, Show ph, Show r, Show l, Eq cn) => GameAction l cn r ph -> Eff es PhaseControl
logAndContinue a = do
  updateGS
  logAction2 a
  continueGame

runGameAction :: forall l r cn ph pl es. (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl, RNG :> es, GameInteract l cn r ph pl :> es, GameRun l cn r ph pl :> es, Log2 :> es, Interface l cn r ph pl :> es) => GameAction l cn r ph -> Eff es PhaseControl
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
      shuffled <- Seq.fromList <$> shuffleRNG (F.toList cards)
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
runGameAction a@(AdvanceTurn t) = do
  assignGameState #nextTurn t
  logAction2 a
  return PCEndTurn
runGameAction a@(EndGame winners) = do
  gs <- getGameState
  scoreFn <- getScore
  scores <- traverse (evalRule . scoreFn) (S.toList (gs ^. #players))
  logWinners (T.intercalate "," (map tshow scores))
  announceWinners winners
  logAction2 a
  return (PCEndGame winners)
runGameAction a@(MakeAnnouncement speaker announcement) = do
  announce speaker announcement
  logAndContinue a

continueGame :: Eff es PhaseControl
continueGame = return PCContinue

-- all control stuff

runFromPhases :: forall l cn r ph pl es. (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl, GameInteract l cn r ph pl :> es, Interface l cn r ph pl :> es, RNG :> es, Log2 :> es, GameRun l cn r ph pl :> es) => [ph] -> Eff es TurnControl
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

playGameTurns :: forall l cn r ph pl es. (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl, RNG :> es, Interface l cn r ph pl :> es, GameInteract l cn r ph pl :> es, Log2 :> es, GameRun l cn r ph pl :> es) => Maybe (GameRule l cn r ph pl ()) -> Eff es (GameState l cn r ph pl, [Player])
playGameTurns setupRule = do
  mapM_ runRuleControl setupRule
  updateGS
  winners <- playGameTurns'
  liftA2 (,) getGameState (pure winners)
  where
    playGameTurns' = do
      gs <- getGameState
      result <- runFromPhases (gs ^. #currentTurn . #turnPhases . to NE.toList)
      case result of
        TEndGame winners -> return winners
        TEndTurn -> do
          t <- useGameState #nextTurn
          logComponent "end of turn"
          assignGameState #currentTurn t
          updateGS
          playGameTurns'

-- Run rule and return appropriate PhaseControl
runRuleControl' :: forall l r cn ph pl es a. (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl, Interface l cn r ph pl :> es, GameInteract l cn r ph pl :> es, GameRun l cn r ph pl :> es, RNG :> es, Log2 :> es) => Free (GameRuleF l cn r ph pl) a -> Eff es PhaseControl
runRuleControl' (Free (Act action next)) = do
  result <- runGameAction action
  case result of
    PCContinue -> runRuleControl' next
    _          -> return result
runRuleControl' (Free (MakeChoice opts k)) = do
  gs <- getGameState
  pl <- choose gs opts
  logChoice (TL.toStrict (encodeToLazyText (gs, pl)))
  runner <- getRunner
  let GameRule run = runner pl
  result <- runRuleControl' run
  case result of
    PCContinue -> runRuleControl' (k pl)
    _          -> return result
runRuleControl' (Free (LookLocation l next)) = do
  shape <- useGameState (#objects . #locations . ftAt l)
  runRuleControl' (next shape)
runRuleControl' (Free (LookCounter cn next)) = do
  counter <- useGameState (#objects . #counters . ftAt cn)
  runRuleControl' (next counter)
runRuleControl' (Free (LookCurrentPhase next)) = useGameState #currentPhase >>= runRuleControl' . next
runRuleControl' (Free (LookCurrentTurnOwner next)) = useGameState (#currentTurn . to (\(Turn p _) -> p)) >>= runRuleControl' . next
runRuleControl' (Free (LookPlayers next)) = useGameState #players >>= runRuleControl' . next
runRuleControl' (Free (LookGameState next)) = getGameState >>= runRuleControl' . next
runRuleControl' (Pure _) = return PCContinue

runRuleControl :: (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl, Interface l cn r ph pl :> es, GameInteract l cn r ph pl :> es, GameRun l cn r ph pl :> es, RNG :> es, Log2 :> es) => GameRule l cn r ph pl a -> Eff es PhaseControl
runRuleControl (GameRule rule) = runRuleControl' rule
