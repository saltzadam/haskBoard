{-# LANGUAGE OverloadedStrings #-}

-- | The game engine. It runs 'GameRule's against the game state, asks the 'Interface'
-- for choices and reports progress. 'applyAction' defines each state change purely
-- and 'playGame' sequences the changes in IO.
module Game.GameE (Env (..), playGame, applyAction, describeAction) where

import Control.Applicative (asum)
import Control.Lens (over, set, to, (&), (.~), (^.))
import Control.Monad.Free (Free (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT, gets, modify, runStateT)
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (first)
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)
import Game.Choose (Interface)
import Game.Constraints (GameCounter, GameLocation, GamePhase, GamePlay, GameResource)
import Game.GameAction (GameAction (..))
import Game.GameState
import Game.Location (LocationShape (..), decrement, increment, inventory, setCounter, swap, transfer, transferCounter)
import Game.Player (Player (..), Turn (..))
import Game.Rules
import Game.Visibility (makeInvisible, makeVisible)
import Log (LogTag (..), Logger)
import ShuffleRNG (shuffleList)
import System.Random (RandomGen, StdGen)
import qualified System.Random as R
import Util (tshow)

-- | Everything the engine needs besides the state.
data Env l cn r ph pl = Env
  { rules :: GameRules l cn r ph pl,
    interface :: Interface l cn r ph pl,
    logger :: Logger
  }
  deriving (Generic)

type EngineM l cn r ph pl = StateT (GameState l cn r ph pl, StdGen) IO

-- | Run setup, then turns until the game ends. Returns the final state and winners.
playGame ::
  (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl) =>
  Env l cn r ph pl ->
  GameState l cn r ph pl ->
  StdGen ->
  IO (GameState l cn r ph pl, [Player])
playGame env gs0 gen0 = do
  (winners, (gsFinal, _)) <- runStateT game (gs0, gen0)
  pure (gsFinal, winners)
  where
    game = do
      mapM_ (runRule env) (env ^. #rules . #setupPhase)
      notifyUpdate env
      turns
    turns = do
      gs <- getGS
      result <- runPhases env (gs ^. #currentTurn . #turnPhases . to NE.toList)
      case result of
        TEndGame winners -> pure winners
        TEndTurn -> do
          logLine env ActionLog "end of turn"
          modifyGS (\s -> s & #currentTurn .~ (s ^. #nextTurn))
          notifyUpdate env
          turns

-- Every phase runs even after an earlier one ends the turn or game, and the first
-- result wins. Changing this could alter multi-phase games. Fix separately.
runPhases ::
  (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl) =>
  Env l cn r ph pl ->
  [ph] ->
  EngineM l cn r ph pl TurnControl
runPhases env phases = fromMaybe TEndTurn . asum <$> traverse handlePhase phases
  where
    handlePhase phase = do
      modifyGS (#currentPhase .~ phase)
      let Phase _ rule = (env ^. #rules . #phases) phase
      result <- runRule env rule
      pure $ case result of
        PCEndTurn -> Just TEndTurn
        PCEndGame winners -> Just (TEndGame winners)
        PCEndPhase -> Nothing
        PCContinue -> Nothing

runRule ::
  forall l cn r ph pl a.
  (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl) =>
  Env l cn r ph pl ->
  GameRule l cn r ph pl a ->
  EngineM l cn r ph pl PhaseControl
runRule env (GameRule rule) = go rule
  where
    go :: Free (GameRuleF l cn r ph pl) a -> EngineM l cn r ph pl PhaseControl
    go (Pure _) = pure PCContinue
    go (Free (Act action next)) = do
      result <- runAction env action
      case result of
        PCContinue -> go next
        _ -> pure result
    go (Free (Choose opts k)) = do
      gs <- getGS
      pl <- liftIO ((env ^. #interface . #choose) gs opts)
      logLine env ChoiceLog (TL.toStrict (encodeToLazyText (gs, pl)))
      result <- runRule env ((env ^. #rules . #playRunner) pl)
      case result of
        PCContinue -> go (k pl)
        _ -> pure result
    go (Free (Look k)) = getGS >>= go . k

-- | Apply an action, then notify players, then log.
runAction ::
  (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl) =>
  Env l cn r ph pl ->
  GameAction l cn r ph ->
  EngineM l cn r ph pl PhaseControl
runAction env action = do
  modify (applyAction action)
  gs <- getGS
  let logAction = mapM_ (logLine env ActionLog) (describeAction action gs)
  case action of
    DoNothing -> pure PCContinue
    EndPhase -> logAction >> pure PCEndPhase
    AdvanceTurn _ -> logAction >> pure PCEndTurn
    EndGame winners -> do
      logLine env WinnersLog (T.intercalate "," (map tshow (M.elems (scoresOf (env ^. #rules) gs))))
      liftIO ((env ^. #interface . #announceWinners) winners)
      logAction
      pure (PCEndGame winners)
    MakeAnnouncement speaker announcement -> do
      liftIO ((env ^. #interface . #announce) speaker announcement)
      notifyUpdate env
      logAction
      pure PCContinue
    _ -> do
      notifyUpdate env
      logAction
      pure PCContinue

notifyUpdate :: Env l cn r ph pl -> EngineM l cn r ph pl ()
notifyUpdate env = do
  gs <- getGS
  liftIO ((env ^. #interface . #update) gs (scoresOf (env ^. #rules) gs))

logLine :: Env l cn r ph pl -> LogTag -> Text -> EngineM l cn r ph pl ()
logLine env tag msg = liftIO ((env ^. #logger) tag msg)

getGS :: EngineM l cn r ph pl (GameState l cn r ph pl)
getGS = gets fst

modifyGS :: (GameState l cn r ph pl -> GameState l cn r ph pl) -> EngineM l cn r ph pl ()
modifyGS f = modify (first f)

-- | Apply one action. Randomness comes from an explicit generator rather than IO.
-- 'EndPhase', 'EndGame', 'MakeAnnouncement' and 'DoNothing' do not change the state.
applyAction ::
  (GameLocation l, GameCounter cn, GameResource r, RandomGen g) =>
  GameAction l cn r ph ->
  (GameState l cn r ph pl, g) ->
  (GameState l cn r ph pl, g)
applyAction action (gs, g) = case action of
  MkTransfer l l' r -> (over (#objects . #locations) (transfer r l l') gs, g)
  MkSwap l l' r r' -> (over (#objects . #locations) (swap r r' l l') gs, g)
  IncrementCounter c -> (over (counter c) increment gs, g)
  DecrementCounter c -> (over (counter c) decrement gs, g)
  SetCounter c v -> (over (counter c) (`setCounter` v) gs, g)
  RollCounter c ->
    let (v, g') = R.randomR (gs ^. counter c . #bounds) g
     in (set (counterVal c) v gs, g')
  TransferCounter cnfrom cnto -> (over (#objects . #counters) (transferCounter cnfrom cnto) gs, g)
  Shuffle l -> case gs ^. location l of
    Deck cards ->
      let (shuffled, g') = shuffleList (F.toList cards) g
       in (set (location l) (Deck (Seq.fromList shuffled)) gs, g')
    _ -> (gs, g)
  MakeVisibleTo p lc -> (over #visibility (\vis -> makeVisible vis p lc) gs, g)
  MakeInvisibleTo p lc -> (over #visibility (\vis -> makeInvisible vis p lc) gs, g)
  AdvanceTurn t -> (set #nextTurn t gs, g)
  DoNothing -> (gs, g)
  EndPhase -> (gs, g)
  EndGame _ -> (gs, g)
  MakeAnnouncement _ _ -> (gs, g)

-- | The log line for an action. It describes the state after the action was applied.
describeAction ::
  (GameLocation l, GameCounter cn, GameResource r) =>
  GameAction l cn r ph ->
  GameState l cn r ph pl ->
  Maybe Text
describeAction action gs = case action of
  DoNothing -> Nothing
  IncrementCounter cn -> Just ("Incremented " <> tshow cn <> " to " <> tshow (gs ^. counterVal cn))
  DecrementCounter cn -> Just ("Decremented " <> tshow cn <> " to " <> tshow (gs ^. counterVal cn))
  SetCounter cn i -> Just ("Set " <> tshow cn <> " to " <> tshow i)
  RollCounter cn -> Just ("Rolled " <> tshow cn <> " to " <> tshow (gs ^. counterVal cn))
  TransferCounter cn cn' -> Just ("Moved one from " <> tshow cn <> " to " <> tshow cn')
  Shuffle l -> Just ("Shuffled " <> tshow l)
  MakeVisibleTo p vd -> Just ("Made " <> tshow vd <> " visible to " <> tshow p)
  MakeInvisibleTo p vd -> Just ("Made " <> tshow vd <> " invisible to " <> tshow p)
  EndPhase -> Just "Ended phase"
  AdvanceTurn (Turn p _) -> Just ("advanced turn to " <> tshow p)
  EndGame winners -> Just ("Game over! Winners: " <> tshow winners)
  MkTransfer l l' r -> Just ("Transfered " <> tshow r <> " from " <> tshow l <> " to " <> tshow l' <> contents l l')
  MkSwap l l' r r' -> Just ("Swapped " <> tshow r <> " and " <> tshow r' <> " between " <> tshow l <> " and " <> tshow l' <> contents l l')
  MakeAnnouncement speaker announcement -> Just (maybe "Nobody" tshow speaker <> " announced: " <> announcement)
  where
    contents l l' = "\n Contents of " <> tshow l <> ": " <> inv l <> "\n Contents of " <> tshow l' <> ": " <> inv l'
    inv l = tshow (inventory (gs ^. location l))
