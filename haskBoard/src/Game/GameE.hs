{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# HLINT ignore "Use forM_" #-}
{-# HLINT ignore "Use =<<" #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Game.GameE where

import Control.Lens
  ( 
    view,
    (^.),
  )
import Data.Bitraversable
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Tree (unfoldForestM)
import Effectful
import Effectful.Crypto.RNG
  ( CryptoRNG (..),
    RNG (..),
  )
import FinitaryMap (ftAt, FakeFinitary)
import GHC.Generics (Generic)
import Log
import Game.Options
import Game.GameNode (GameAction (..), GameNode)
import Game.Location (LocationShape (..), decrement, increment, inventory, setCounter, transfer)
import TreeMonad
import Game.Visibility (makeVisible, makeInvisible)
import Util (getNext)
import System.Random.Shuffle (shuffle)
import qualified Data.Foldable as F
import Control.Monad (replicateM, void)
import qualified Data.Sequence as Seq
import Game.GameState
import Game.Choose
import Control.Monad.Trans.Reader (ReaderT, Reader)
import qualified Effectful.State.Static.Shared as State 
import Control.Monad.Reader (runReader)

-- TODO: export list

-- TODO: (lower) abstract out log
-- TODO: (fun) some kind of history besides log


-- thanks /u/typedbyte
-- https://www.reddit.com/r/haskell/comments/10ql43j/monthly_hask_anything_february_2023/jabrmxk/

logAction2 :: (ObserveGame l cn r ph pl i es, Log2 :> es, Show cn, Show ph, Show val, Ord r, Eq l, Show r, Show l) => GameAction l cn r ph -> val -> Eff es ()
logAction2 (IncrementCounter cn) val = logComponent (T.pack $ "Incremented " ++ show cn ++ " to " ++ show val)
logAction2 (DecrementCounter cn) val = logComponent (T.pack $ "Decremented " ++ show cn ++ " to " ++ show val)
logAction2 (SetCounter cn i) _ = logComponent (T.pack $ "Set " ++ show cn ++ " to " ++ show i)
logAction2 (RollCounter cn) val = logComponent (T.pack $ "Rolled " ++ show cn ++ " to " ++ show val)
logAction2 (Shuffle l) _ = logComponent (T.pack $ "Shuffled " ++ show l)
logAction2 (ChangePhase ph) _ = logComponent (T.pack $ "Changed phase to " ++ show ph)
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

-- logAction2' ::
--   ( Log2 :> es,
--     Ord r,
--     Show cn,
--     Show ph,
--     Show val,
--     Show r,
--     Show l,
--     Eq l
--   ) =>
--   GameAction l cn r ph ->
--   val ->
--   Eff (GameInteract l cn r ph pl i : es) ()
-- logAction2' action val = liftObserve (logAction2 action val)


act :: forall l r cn ph pl i es. (Ord l, Ord r, RNG :> es, ObserveGame l cn r ph pl i es, Eq cn, Show ph, Show cn, Show l, Show r, Log2 :> es, Eq ph) => GameAction l cn r ph -> Eff es (Maybe (GameControl ph))
act DoNothing = continueGame
act a@(MkTransfer l l' r) =
  modifyingGameState (#objects . #locations) (transfer r l l')
    >> inject (logAction2 a ' ')
    >> continueGame
act a@(IncrementCounter c) =
  modifyingGameState (counter c) increment
    >> useGameState (counter c . #val)
    >>= inject . logAction2 a
    >> continueGame
act a@(DecrementCounter c) =
  modifyingGameState (counter c) decrement
    >> useGameState (counter c . #val)
    >>= inject . logAction2 a
    >> continueGame
act a@(SetCounter c v) =
  modifyingGameState (counter c) (`setCounter` v)
    >> useGameState (counter c . #val)
    >>= inject . logAction2 a
    >> continueGame
act a@(RollCounter c) = do
  (bl, bu) <- useGameState (counter c . #bounds)
  newVal <- randomR (bl, bu)
  assignGameState (counterVal c) newVal
  _ <- useGameState (counterVal c) >>= inject . logAction2 a
  continueGame
act a@(Shuffle l) = do
    loc <- useGameState (#objects . #locations . ftAt l)
    case loc of
      Deck cards -> do
        uniformSample <- replicateM (length cards - 1) (randomR (0, length cards - 1))
        let shuffled = Seq.fromList $ shuffle (F.toList cards) uniformSample
        assignGameState (#objects . #locations . ftAt l) (Deck shuffled)
      _ -> pure ()
    inject (logAction2 a ' ')
    continueGame

act a@(MakeVisibleTo p lc) =
    modifyVisibility (\vis -> makeVisible vis p lc)
    >> inject (logAction2 a ' ')
    >> continueGame
act a@(MakeInvisibleTo p lc) =
    modifyVisibility (\vis -> makeInvisible vis p lc)
    >> inject (logAction2 a ' ')
    >> continueGame
act a@EndPhase = do
    phase <- useGameState #currentPhase
    turn <- useGameState #currentTurn
    let nextPhase = getNext phase (turnPhases turn)
    inject (logAction2 a ' ')
    case nextPhase of
      Just aPhase -> act (ChangePhase aPhase)
      Nothing -> act AdvanceTurn
act a@AdvanceTurn = do
    turn <- useGameState #currentTurn
    turns <- useGameState #turns
    turner <- useGameState #nextTurn
    let nextTurn = turner turn turns
    assignGameState #currentTurn nextTurn
    inject (logAction2 a ' ')
    act (ChangePhase (NE.head (turnPhases nextTurn)))


act a@(ChangePhase ph) =
  assignGameState #currentPhase ph
    >> inject (logAction2 a (show ph))
    >> return (Just $ ChangePhaseTo ph)
act a@(EndGame _) = inject (logAction2 a ' ') >> return (Just End)


chooseNode :: forall l cn r ph pl i es. (Choosing :> es, GameInteract l cn r ph pl i :> es, Show pl, Show i, Show l, Show r, Show cn, Show ph, Log2 :> es) => Eff es (Options pl i) -> Eff es [GameNode l cn r ph pl i] 
chooseNode cs =
  let cs' = cs
   in do
       gs <- getGameState
       runner <- getRunner
       runner gs <$>
         ( do
                options <- cs'
                logGame (T.pack ("Choosing from " ++ displayOptions options))
                c <- choose options
                logGame (T.pack ("Chose " ++ show c))
                return c
            )


runNode :: forall l r cn ph pl es i. (Ord l, Ord r, Ord cn, FakeFinitary cn, Choosing :> es, ObserveGame l cn r ph pl i es, RNG :> es, Show ph, Show cn, Show l, Show r, Show pl, Show i, Log2 :> es, Eq ph, BroadcastState l cn r ph pl i :> es) => GameNode l cn r ph pl i -> Eff es (Either (GameControl ph) [Eff es [GameNode l cn r ph pl i]])
runNode aNode = maybeLeftToEmptyRight <$> bitraverse handleAction handleChoice (view #node aNode)
  where
    handleAction :: GameAction l cn r ph -> Eff es (Maybe (GameControl ph))
    handleAction action = do
        a <- inject . act $ action
        broadcastState
        return a


    -- TODO: layers of Monad stuff that don't need to be here
    handleChoice :: Options pl i -> Eff es [Eff es [GameNode l cn r ph pl i]]
    handleChoice = pure . pure . inject . chooseNode . inject . pure

    maybeLeftToEmptyRight :: Monoid b => Either (Maybe a) b -> Either a b
    maybeLeftToEmptyRight (Left Nothing) = Right mempty
    maybeLeftToEmptyRight (Left (Just i)) = Left i
    maybeLeftToEmptyRight (Right x) = Right x

runFromSeeds2 :: forall l r cn ph pl es i. (Ord l, Ord r, Ord cn, FakeFinitary cn, ObserveGame l cn r ph pl i es, Choosing :> es, RNG :> es, Show ph, Show cn, Show l, Show r, Show pl, Show i, Log2 :> es, Eq ph, BroadcastState l cn r ph pl i :> es) => [Eff es [GameNode l cn r ph pl i]] -> Eff es ()
runFromSeeds2 nodes = do
  theTree <- fmap (fmap concat) . unTreeMonad . unfoldForestM unfoldFunc $ nodes
  case theTree of
    Left End -> pure ()
    Left (ChangePhaseTo ph) -> do
      phaser <- useGameState #phases
      let thisPhase = phaser ph
      gs <- getGameState
      runFromSeeds2 (inject <$> [pure (getPhaseNodes thisPhase gs)])
    Right _ -> error "oops more nodes" -- TODO: make this unrepresentable!!
  where
    unfoldFunc ::
      Eff es [GameNode l cn r ph pl i] ->
      TreeMonad l cn r ph pl i es (GameNode l cn r ph pl i, [Eff es [GameNode l cn r ph pl i]])
    unfoldFunc effNodes = TreeMonad $ do
      nodes' <- effNodes
      unfolded <- traverse unfoldFunc' nodes'
      let unfolded' = sequence unfolded
      return unfolded'

    unfoldFunc' ::
      GameNode l cn r ph pl i ->
      Eff
        es
        ( Either
            (GameControl ph)
            (GameNode l cn r ph pl i, [Eff es [GameNode l cn r ph pl i]])
        )
    unfoldFunc' aNode = do
      result <- runNode aNode
      return ((aNode,) <$> result)

playGame :: forall l cn r ph pl es i. (Ord l, Ord r, Ord cn, FakeFinitary cn, RNG :> es, Choosing :> es, ObserveGame l cn r ph pl i es, Show ph, Show cn, Show l, Show r, Show pl, Show i, Log2 :> es, Eq ph, BroadcastState l cn r ph pl i :> es) => Eff es (GameState l cn r ph pl i)
playGame = do
  gs <- getGameState
  let phases = gs ^. #phases
  currentPhase <- useGameState #currentPhase
  let newNodes = inject <$> [pure $ getPhaseNodes (phases currentPhase) gs]
  runFromSeeds2 newNodes
  getGameState

data PhaseControl = PCEndPhase | PCEndTurn | PCEndGame deriving (Eq, Ord, Show, Generic)

runPhaseNodes :: forall l r cn ph pl es i. (Ord l, Ord r, Ord cn, FakeFinitary cn, ObserveGame l cn r ph pl i es, Choosing :> es, RNG :> es, Show ph, Show cn, Show l, Show r, Show pl, Show i, Log2 :> es, Eq ph, BroadcastState l cn r ph pl i :> es) => [Eff es [GameNode l cn r ph pl i]] -> Eff es PhaseControl
runPhaseNodes nodes = do
  theTree <- fmap (fmap concat) . unTreeMonad . unfoldForestM unfoldFunc $ nodes
  case theTree of
    Left End -> pure PCEndGame
    Left (ChangePhaseTo ph) -> do
      phaser <- useGameState #phases
      let thisPhase = phaser ph
      gs <- getGameState
      runPhaseNodes (inject <$> [pure $ getPhaseNodes thisPhase gs])
    Right _ -> pure PCEndTurn
    where
    unfoldFunc ::
      Eff es [GameNode l cn r ph pl i] ->
      TreeMonad l cn r ph pl i es (GameNode l cn r ph pl i, [Eff es [GameNode l cn r ph pl i]])
    unfoldFunc effNodes = TreeMonad $ do
      nodes' <- effNodes
      unfolded <- traverse unfoldFunc' nodes'
      let unfolded' = sequence unfolded
      return unfolded'

    unfoldFunc' ::
      GameNode l cn r ph pl i ->
      Eff
        es
        ( Either
            (GameControl ph)
            (GameNode l cn r ph pl i, [Eff es [GameNode l cn r ph pl i]])
        )
    unfoldFunc' aNode = do
      result <- runNode aNode
      return ((aNode,) <$> result)

data TurnControl = TEndTurn | TEndGame deriving (Eq, Ord, Show, Generic)

runFromPhases :: (GameInteract l cn r ph pl i :> es, Ord l, FakeFinitary cn, Show ph, Choosing :> es, RNG :> es, Log2 :> es, Ord r, Eq ph, Show cn, Ord cn, Show l, Show r, Show pl, Show i, BroadcastState l cn r ph pl i :> es) => [ph] -> Eff es TurnControl
runFromPhases (phase:theRest) = do
    assignGameState #currentPhase phase
    phases <- useGameState #phases
    gs <- getGameState
    let newNodes = inject <$> [pure $ getPhaseNodes (phases phase) gs]
    result <- runPhaseNodes newNodes
    case result of
      PCEndPhase -> runFromPhases theRest
      PCEndTurn -> return TEndTurn
      PCEndGame -> return TEndGame
runFromPhases [] = return TEndTurn


runTurns :: (GameInteract l cn r ph pl i :> es, FakeFinitary cn, Choosing :> es, RNG :> es, Log2 :> es, Ord l, Ord r, Eq ph, Ord cn, Show ph, Show cn, Show l, Show r, Show pl, Show i, BroadcastState l cn r ph pl i :> es) => Turn ph -> Eff es TurnControl
runTurns turn  = do
    let phases = NE.toList (turn ^. #turnPhases)
    runFromPhases phases


playGameTurns :: forall l cn r ph pl es i. (Ord l, Ord r, Ord cn, FakeFinitary cn, RNG :> es, Choosing :> es, ObserveGame l cn r ph pl i es, Show ph, Show cn, Show l, Show r, Show pl, Show i, Log2 :> es, Eq ph, BroadcastState l cn r ph pl i :> es) => Eff es (GameState l cn r ph pl i)
playGameTurns = do
    getSetupNodes <- useGame #setup
    gs <- getGameState
    let setupNodes = pure $ getSetupNodes gs
    _ <- runPhaseNodes [inject setupNodes]
    void playGameTurns' 
    getGameState
        where
            playGameTurns' = do
                gs <- getGameState
                let currentTurn = gs ^. #currentTurn
                result <- runTurns currentTurn
                case result of
                  TEndGame -> return TEndGame
                  TEndTurn -> act AdvanceTurn >> playGameTurns'


playGivenNodes :: forall l cn r ph pl es i. (Ord l, Ord r, Ord cn, FakeFinitary cn, RNG :> es, Choosing :> es, ObserveGame l cn r ph pl i es, Show ph, Show cn, Show l, Show r, Show pl, Show i, Log2 :> es, Eq ph, BroadcastState l cn r ph pl i :> es) => [Eff es [GameNode l cn r ph pl i]] -> Eff es (GameState l cn r ph pl i)
playGivenNodes nodes = do
  runFromSeeds2 nodes
  getGameState

readerToEff :: (State.State r :> es) => Reader r a -> Eff es a
readerToEff reader = State.state (\r -> (runReader reader r, r))
