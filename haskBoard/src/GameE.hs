{-# LANGUAGE AllowAmbiguousTypes #-}
{-# HLINT ignore "Use void" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# HLINT ignore "Use forM_" #-}
{-# HLINT ignore "Use =<<" #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module GameE where

import Control.Lens
  ( Getting,
    Lens',
    makeFields,
    over,
    set,
    to,
    view,
    (^.),
  )
import Control.Lens.Combinators (ASetter)
import Count
import Data.Bitraversable
import Data.Finitary (Finitary)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Text as T
import Data.Tree (unfoldForestM)
import Effectful
import Effectful.Crypto.RNG
  ( CryptoRNG (..),
    RNG (..),
    newCryptoRNGState,
    runCryptoRNG,
  )
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.Dispatch.Static (SideEffects (..), StaticRep, evalStaticRep, getStaticRep, putStaticRep)
import FinitaryMap (ftAt)
import GHC.Base (Applicative (..), NonEmpty)
import GHC.Generics (Generic)
import Game.Log
import Game.Options
import Game.Player (Player)
import GameNode (GameAction (..), GameNode)
import Location (Counter, GameObjects, LocationShape (..), decrement, increment, inventory, setCounter, transfer, findResourceWithin)
import Text.Read (readMaybe)
import TreeMonad
import Visibility (VisibilityMap, makeVisible, makeInvisible)
import Util (getNext)
import System.Random.Shuffle (shuffle)
import qualified Data.Foldable as F
import Control.Monad (replicateM)
import qualified Data.Sequence as Seq

-- TODO: export list

-- TODO: (lower) abstract out log
-- TODO: (fun) some kind of history besides log


data Turn phaseName = Turn {owner :: Player,
    turnPhases :: NE.NonEmpty phaseName} deriving (Eq, Ord, Show, Generic)

data Phase phaseName l cn r playName i = Phase
  { name :: phaseName,
    seedNodes :: [Eff '[GameInteract 'Observe l cn r phaseName playName i] [GameNode l cn r phaseName playName i]]
  }
  deriving (Generic)

getPhaseNodes :: Phase phaseName l cn r playName i -> [Eff '[GameInteract 'Observe l cn r phaseName playName i] [GameNode l cn r phaseName playName i]]
getPhaseNodes (Phase _ seedNodes') = seedNodes'

-- thanks /u/typedbyte
-- https://www.reddit.com/r/haskell/comments/10ql43j/monthly_hask_anything_february_2023/jabrmxk/

data Mode = Observe | Modify deriving (Eq, Ord, Show, Generic)

type PlayRunner' l cn r ph pl i mode = pl -> [Eff '[GameInteract mode l cn r ph pl i] [GameNode l cn r ph pl i]]

type PlayRunner l cn r ph pl i = PlayRunner' l cn r ph pl i 'Observe

data Game l cn r ph pl i = Game
  { gameState :: GameState l cn r ph pl i,
    playRunner :: PlayRunner l cn r ph pl i,
    visibility :: VisibilityMap l cn,
    setup :: Eff '[GameInteract 'Observe l cn r ph pl i] [GameNode l cn r ph pl i]
  }
  deriving (Generic)

data GameInteract (mode :: Mode) l cn r ph pl i :: Effect

type instance DispatchOf (GameInteract mode l cn r ph pl i) = 'Static 'NoSideEffects

newtype instance StaticRep (GameInteract mode l cn r ph pl i) = GameInteract (Game l cn r ph pl i)

liftStaticRepMode :: StaticRep (GameInteract 'Observe l cn r ph pl i) -> StaticRep (GameInteract 'Modify l cn r ph pl i)
liftStaticRepMode (GameInteract g) = GameInteract g

maybeUnsafeUnliftStaticRepMode :: StaticRep (GameInteract 'Modify l cn r ph pl i) -> StaticRep (GameInteract 'Observe l cn r ph pl i)
maybeUnsafeUnliftStaticRepMode (GameInteract g) = GameInteract g

runGameInteract ::
  forall l cn r ph pl i mode es a.
  GameState l cn r ph pl i ->
  PlayRunner l cn r ph pl i ->
  VisibilityMap l cn ->
  Eff '[GameInteract 'Observe l cn r ph pl i] [GameNode l cn r ph pl i] ->
  Eff (GameInteract mode l cn r ph pl i : es) a ->
  Eff es a
runGameInteract gd pr vis setup = evalStaticRep (GameInteract (Game gd pr vis setup) :: StaticRep (GameInteract mode l cn r ph pl i))

askGame :: GameInteract mode l cn r ph pl i :> es => Eff es (GameState l cn r ph pl i)
askGame = do
  GameInteract (Game gd _ _ _) <- getStaticRep
  return gd

liftObserve :: Eff (GameInteract 'Observe l cn r ph pl i : es) a -> Eff (GameInteract 'Modify l cn r ph pl i : es) a
liftObserve eff = do
  GameInteract g <- getStaticRep
  raise $ evalStaticRep (GameInteract g) eff

unsafeProjToObserve :: Eff (GameInteract 'Modify l cn r ph pl i : es) a -> Eff (GameInteract 'Observe l cn r ph pl i : es) a
unsafeProjToObserve eff = do
  GameInteract g <- getStaticRep
  raise $ evalStaticRep (GameInteract g) eff

type ObserveGame l cn r ph pl i es = GameInteract 'Observe l cn r ph pl i :> es

type ModifyGame l cn r ph pl i es = GameInteract 'Modify l cn r ph pl i :> es

data GameState l cn r ph pl i = GameState
  { players :: Set Player,
    objects :: GameObjects l cn r,
    currentPhase :: ph,
    phases :: ph -> Phase ph l cn r pl i,
    turns :: NonEmpty (Turn ph),
    currentTurn :: Turn ph,
    nextTurn :: Turn ph -> NonEmpty (Turn ph) -> Turn ph
  }
  deriving (Generic)

-- playerTurn p = Turn p (NE.singleton (NMTurn p))

askRunner :: forall l cn r ph pl i es. (GameInteract 'Observe l cn r ph pl i :> es) => Eff es (PlayRunner l cn r ph pl i)
askRunner = do
  GameInteract (Game _ pr _ _) <- getStaticRep @(GameInteract 'Observe l cn r ph pl i)
  return pr

getRunner :: forall l cn r ph pl i es. (GameInteract 'Modify l cn r ph pl i :> es) => Eff es (PlayRunner l cn r ph pl i)
getRunner = do
  GameInteract (Game _ pr _ _) <- getStaticRep @(GameInteract 'Modify l cn r ph pl i)
  return pr

modifyGameState :: forall l cn r ph pl i es. (GameInteract 'Modify l cn r ph pl i :> es) => (GameState l cn r ph pl i -> GameState l cn r ph pl i) -> Eff es ()
modifyGameState f = do
  GameInteract (Game gs gr vis setup) <- getStaticRep @(GameInteract 'Modify l cn r ph pl i)
  putStaticRep (GameInteract (Game (f gs) gr vis setup))

modifyingGameState :: (GameInteract 'Modify l cn r ph pl i :> es) => ASetter (GameState l cn r ph pl i) (GameState l cn r ph pl i) a b -> (a -> b) -> Eff es ()
modifyingGameState o = modifyGameState . over o

assignGameState :: (GameInteract 'Modify l cn r ph pl i :> es) => ASetter (GameState l cn r ph pl i) (GameState l cn r ph pl i) a b -> b -> Eff es ()
assignGameState l b = modifyGameState (set l b)

getsGameState :: forall mode l cn r ph pl i es b. (GameInteract mode l cn r ph pl i :> es) => (GameState l cn r ph pl i -> b) -> Eff es b
getsGameState f = do
  GameInteract (Game gs _ _ _) <- getStaticRep @(GameInteract mode l cn r ph pl i)
  return (f gs)

getGameState :: forall mode l cn r ph pl i es. (GameInteract mode l cn r ph pl i :> es) => Eff es (GameState l cn r ph pl i)
getGameState = getsGameState id

useGameState :: (GameInteract mode l cn r ph pl i :> es) => Getting b (GameState l cn r ph pl i) b -> Eff es b
useGameState o = getsGameState (view o)

getsVisibility :: forall mode l cn r ph pl i es b. (GameInteract mode l cn r ph pl i :> es) => (VisibilityMap l cn-> b) -> Eff es b
getsVisibility f = do
  GameInteract (Game _ _ vis _) <- getStaticRep @(GameInteract mode l cn r ph pl i)
  return (f vis)

getVisibility :: forall mode l cn r ph pl i es. (GameInteract mode l cn r ph pl i :> es) => Eff es (VisibilityMap l cn)
getVisibility = getsVisibility id

modifyVisibility :: forall l cn r ph pl i es. (GameInteract 'Modify l cn r ph pl i :> es) => (VisibilityMap l cn ->  VisibilityMap l cn) -> Eff es ()
modifyVisibility f = do
  GameInteract (Game gs gr vis setup) <- getStaticRep @(GameInteract 'Modify l cn r ph pl i)
  putStaticRep (GameInteract (Game gs gr (f vis) setup))

useVisibility :: (GameInteract mode l cn r ph pl i :> es) => Getting b (VisibilityMap l cn) b -> Eff es b
useVisibility o = getsVisibility (view o)

assignVisibility :: (GameInteract 'Modify l cn r ph pl i :> es) => ASetter (VisibilityMap l cn) (VisibilityMap l cn) a b -> b -> Eff es ()
assignVisibility l b = modifyVisibility (set l b)

getsSetup :: forall mode l cn r ph pl i es b. (GameInteract mode l cn r ph pl i :> es) => (Eff '[GameInteract 'Observe l cn r ph pl i] [GameNode l cn r ph pl i] -> b) -> Eff es b
getsSetup f = do
  GameInteract (Game _ _ _ setup) <- getStaticRep @(GameInteract mode l cn r ph pl i)
  return (f setup)

getSetup :: forall mode l cn r ph pl i es. (GameInteract mode l cn r ph pl i :> es) => Eff es (Eff '[GameInteract 'Observe l cn r ph pl i] [GameNode l cn r ph pl i])
getSetup = getsSetup id



observe :: Game l cn r ph pl i -> Eff '[GameInteract mode l cn r ph pl i] a -> a
observe g eff = runPureEff . runGameInteract (g ^. #gameState) (g ^. #playRunner) (g ^. #visibility) (g^. #setup) $ eff

showInventory :: (GameInteract 'Observe l cn r ph pl i :> es, Eq l, Show r, Ord r) => l -> Eff es String
showInventory l = show . inventory <$> useGameState (location l)

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
  invl <- showInventory l
  invl' <- showInventory l'
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

logAction2' ::
  ( Log2 :> es,
    Ord r,
    Show cn,
    Show ph,
    Show val,
    Show r,
    Show l,
    Eq l
  ) =>
  GameAction l cn r ph ->
  val ->
  Eff (GameInteract 'Modify l cn r ph pl i : es) ()
logAction2' action val = liftObserve (logAction2 action val)

act :: forall l r cn ph pl i es. (Ord l, Ord r, RNG :> es, ModifyGame l cn r ph pl i es, Eq cn, Show ph, Show cn, Show l, Show r, Log2 :> es, Eq ph) => GameAction l cn r ph -> Eff es (Maybe (GameControl ph))
act DoNothing = continueGame
act a@(MkTransfer l l' r) =
  modifyingGameState (#objects . #locations) (transfer r l l')
    >> inject (logAction2' a ' ')
    >> continueGame
act a@(IncrementCounter c) =
  modifyingGameState (counter c) increment
    >> useGameState (counter c . #val)
    >>= inject . logAction2' a
    >> continueGame
act a@(DecrementCounter c) =
  modifyingGameState (counter c) decrement
    >> useGameState (counter c . #val)
    >>= inject . logAction2' a
    >> continueGame
act a@(SetCounter c v) =
  modifyingGameState (counter c) (`setCounter` v)
    >> useGameState (counter c . #val)
    >>= inject . logAction2' a
    >> continueGame
act a@(RollCounter c) = do
  (bl, bu) <- useGameState (counter c . #bounds)
  newVal <- randomR (bl, bu)
  assignGameState (counterVal c) newVal
  _ <- useGameState (counterVal c) >>= inject . logAction2' a
  continueGame
act a@(Shuffle l) = do
    loc <- useGameState (#objects . #locations . ftAt l)
    case loc of
      Deck cards -> do
        uniformSample <- replicateM (length cards - 1) (randomR (0, length cards - 1))
        let shuffled = Seq.fromList $ shuffle (F.toList cards) uniformSample
        assignGameState (#objects . #locations . ftAt l) (Deck shuffled)
      _ -> pure ()
    inject (logAction2' a ' ')
    continueGame

act a@(MakeVisibleTo p lc) =
    modifyVisibility (\vis -> makeVisible vis p lc)
    >> inject (logAction2' a ' ')
    >> continueGame
act a@(MakeInvisibleTo p lc) =
    modifyVisibility (\vis -> makeInvisible vis p lc)
    >> inject (logAction2' a ' ')
    >> continueGame
act a@EndPhase = do
    phase <- useGameState #currentPhase
    turn <- useGameState #currentTurn
    let nextPhase = getNext phase (turnPhases turn)
    inject (logAction2' a ' ')
    case nextPhase of
      Just aPhase -> act (ChangePhase aPhase)
      Nothing -> act AdvanceTurn
act a@AdvanceTurn = do
    turn <- useGameState #currentTurn
    turns <- useGameState #turns
    turner <- useGameState #nextTurn
    let nextTurn = turner turn turns
    assignGameState #currentTurn nextTurn
    inject (logAction2' a ' ')
    act (ChangePhase (NE.head (turnPhases nextTurn)))


act a@(ChangePhase ph) =
  assignGameState #currentPhase ph
    >> inject (logAction2' a (show ph))
    >> return (Just $ ChangePhaseTo ph)
act a@(EndGame _) = inject (logAction2' a ' ') >> return (Just End)

counter :: Eq cn => cn -> Lens' (GameState l cn r ph pl i) Counter
counter c = #objects . #counters . ftAt c

counterVal :: Eq cn => cn -> Lens' (GameState l cn r ph pl i) (Cnt Int)
counterVal c = counter c . #val

location :: Eq l => l -> Lens' (GameState l cn r ph pl i) (LocationShape r)
location l = #objects . #locations . ftAt l

makeFields ''GameState
makeFields ''Game
makeFields ''Phase

chooseNode :: forall l cn r ph pl i es. (Choosing :> es, GameInteract 'Observe l cn r ph pl i :> es, Show pl, Show i, Show l, Show r, Show cn, Show ph, Log2 :> es) => Eff es (Options pl i) -> Eff es [Eff '[GameInteract 'Observe l cn r ph pl i] [GameNode l cn r ph pl i]]
chooseNode cs =
  let cs' = cs
   in askRunner
        <*> ( do
                options <- cs'
                logGame (T.pack ("Choosing from " ++ displayOptions options))
                c <- choose options
                logGame (T.pack ("Chose " ++ show c))
                return c
            )

data Choosing :: Effect where
  Choose :: GameState l cn r ph pl i -> Options pl i -> Choosing m pl

type instance DispatchOf Choosing = 'Dynamic

choose :: forall l cn r ph pl mode es i. (Choosing :> es, GameInteract mode l cn r ph pl i :> es) => Options pl i -> Eff es pl
choose cs = askGame >>= \g -> send (Choose g cs)

chooseFirst :: forall es pl. Eff (Choosing : es) pl -> Eff es pl
chooseFirst = interpret $ \_ -> \case
  Choose _ cs -> return (cs ^. #legal . to NE.head)

chooseRandom :: (RNG :> es) => Eff (Choosing : es) pl -> Eff es pl
chooseRandom = interpret $ \_ -> \case
  Choose _ cs' ->
    let cs = cs' ^. #legal
        choice = randomR (0, length cs - 1)
     in fmap (cs NE.!!) choice

chooseBasicInput :: forall pl es. (IOE :> es) => Eff (Choosing : es) pl -> Eff es pl
chooseBasicInput = interpret $ \_ -> \case
  Choose _ cs' -> do
    let cs = cs' ^. #legal . to NE.toList
    liftIO $ loopChoice cs
  where
    loopChoice cs = do
      c <- liftIO getChar
      case readMaybe [c] :: Maybe Int of
        Nothing -> putStrLn "couldn't parse" >> loopChoice cs
        Just i -> case listToMaybe (drop (i - 1) cs) of
          Just pl -> return pl
          Nothing -> putStrLn "couldn't find" >> loopChoice cs

runNode :: forall l r cn ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, Choosing :> es, ModifyGame l cn r ph pl i es, RNG :> es, Show ph, Show cn, Show l, Show r, Show pl, Show i, Log2 :> es, Eq ph) => GameNode l cn r ph pl i -> Eff es (Either (GameControl ph) [Eff es [GameNode l cn r ph pl i]])
runNode aNode = maybeLeftToEmptyRight <$> bitraverse handleAction handleChoice (view #node aNode)
  where
    handleAction :: GameAction l cn r ph -> Eff es (Maybe (GameControl ph))
    handleAction = inject . act

    handleChoice :: Options pl i -> Eff es [Eff es [GameNode l cn r ph pl i]]
    handleChoice = fmap (fmap (inject . liftObserve)) . inject . liftObserve . chooseNode . inject . unsafeProjToObserve . pure

    maybeLeftToEmptyRight :: Monoid b => Either (Maybe a) b -> Either a b
    maybeLeftToEmptyRight (Left Nothing) = Right mempty
    maybeLeftToEmptyRight (Left (Just i)) = Left i
    maybeLeftToEmptyRight (Right x) = Right x

runFromSeeds2 :: forall l r cn ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, ModifyGame l cn r ph pl i es, Choosing :> es, RNG :> es, Show ph, Show cn, Show l, Show r, Show pl, Show i, Log2 :> es, Eq ph) => [Eff es [GameNode l cn r ph pl i]] -> Eff es ()
runFromSeeds2 nodes = do
  theTree <- fmap (fmap concat) . unTreeMonad . unfoldForestM unfoldFunc $ nodes
  case theTree of
    Left End -> pure ()
    Left (ChangePhaseTo ph) -> do
      phaser <- useGameState #phases
      let thisPhase = phaser ph
      runFromSeeds2 (inject . liftObserve <$> getPhaseNodes thisPhase)
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

playGame :: forall l cn r ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, RNG :> es, Choosing :> es, ModifyGame l cn r ph pl i es, Show ph, Show cn, Show l, Show r, Show pl, Show i, Log2 :> es, Eq ph) => Eff es (GameState l cn r ph pl i)
playGame = do
  gs <- getGameState
  let phases = gs ^. #phases
  currentPhase <- useGameState #currentPhase
  let newNodes = inject . liftObserve <$> getPhaseNodes (phases currentPhase)
  runFromSeeds2 newNodes
  getGameState

data PhaseControl = PCEndPhase | PCEndTurn | PCEndGame deriving (Eq, Ord, Show, Generic)

runPhaseNodes :: forall l r cn ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, ModifyGame l cn r ph pl i es, Choosing :> es, RNG :> es, Show ph, Show cn, Show l, Show r, Show pl, Show i, Log2 :> es, Eq ph) => [Eff es [GameNode l cn r ph pl i]] -> Eff es PhaseControl
runPhaseNodes nodes = do
  theTree <- fmap (fmap concat) . unTreeMonad . unfoldForestM unfoldFunc $ nodes
  case theTree of
    Left End -> pure PCEndGame
    Left (ChangePhaseTo ph) -> do
      phaser <- useGameState #phases
      let thisPhase = phaser ph
      runPhaseNodes (inject . liftObserve <$> getPhaseNodes thisPhase)
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

runFromPhases :: (GameInteract 'Modify l cn r ph pl i :> es, Ord l, Finitary cn, Show ph, Choosing :> es, RNG :> es, Log2 :> es, Ord r, Eq ph, Show cn, Ord cn, Show l, Show r, Show pl, Show i) => [ph] -> Eff es TurnControl
runFromPhases (phase:theRest) = do
    assignGameState #currentPhase phase
    phases <- useGameState #phases
    let newNodes = inject . liftObserve <$> getPhaseNodes (phases phase)
    result <- runPhaseNodes newNodes
    case result of
      PCEndPhase -> runFromPhases theRest
      PCEndTurn -> return TEndTurn
      PCEndGame -> return TEndGame
runFromPhases [] = return TEndTurn


runTurns :: (GameInteract 'Modify l cn r ph pl i :> es, Finitary cn, Choosing :> es, RNG :> es, Log2 :> es, Ord l, Ord r, Eq ph, Ord cn, Show ph, Show cn, Show l, Show r, Show pl, Show i) => Turn ph -> Eff es TurnControl
runTurns turn  = do
    let phases = NE.toList (turn ^. #turnPhases)
    runFromPhases phases


playGameTurns :: forall l cn r ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, RNG :> es, Choosing :> es, ModifyGame l cn r ph pl i es, Show ph, Show cn, Show l, Show r, Show pl, Show i, Log2 :> es, Eq ph) => Eff es TurnControl
playGameTurns = do
    setupNodes <- getSetup
    _ <- runPhaseNodes [inject . liftObserve $ setupNodes]
    playGameTurns' 
        where
            playGameTurns' = do
                gs <- getGameState
                let currentTurn = gs ^. #currentTurn
                result <- runTurns currentTurn
                case result of
                  TEndGame -> return TEndGame
                  TEndTurn -> act AdvanceTurn >> playGameTurns'


playGivenNodes :: forall l cn r ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, RNG :> es, Choosing :> es, ModifyGame l cn r ph pl i es, Show ph, Show cn, Show l, Show r, Show pl, Show i, Log2 :> es, Eq ph) => [Eff es [GameNode l cn r ph pl i]] -> Eff es (GameState l cn r ph pl i)
playGivenNodes nodes = do
  runFromSeeds2 nodes
  getGameState

action :: (Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l, Show r, Show pl, Show i, Eq ph) => GameState l cn r ph pl i -> PlayRunner l cn r ph pl i -> VisibilityMap l cn -> Eff '[GameInteract 'Observe l cn r ph pl i] [GameNode l cn r ph pl i] -> IO (GameState l cn r ph pl i)
action gdata playRunner vis setup = do
  gen <- newCryptoRNGState
  runEff . runGameInteract gdata playRunner vis (inject setup) . runCryptoRNG gen . chooseRandom . logStdOut DebugLevel $ playGame

actionTurns :: (Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l,
 Show r, Show pl, Show i, Eq ph) =>
  GameState l cn r ph pl i
  -> PlayRunner l cn r ph pl i
  -> VisibilityMap l cn
  -> Eff '[GameInteract 'Observe l cn r ph pl i] [GameNode l cn r ph pl i] 
  -> IO TurnControl
actionTurns gdata playRunner vis setup = do
    gen <- newCryptoRNGState
    runEff . runGameInteract gdata playRunner vis setup . runCryptoRNG gen . chooseRandom . logStdOut DebugLevel $ playGameTurns

----- Condition, perhaps soon to be Observation?
-- type Condition l cn r ph pl i es a = Eff es a

runNodesAgainstState :: (Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l, Show r, Show pl, Show i, Eq ph) => GameState l cn r ph pl i -> PlayRunner l cn r ph pl i -> VisibilityMap l cn 
   -> Eff '[GameInteract 'Observe l cn r ph pl i] [GameNode l cn r ph pl i]
                     -> [GameNode l cn r ph pl i] -> IO (GameState l cn r ph pl i)
runNodesAgainstState game playRunner vis setup nodes = do
  gen <- newCryptoRNGState
  runEff . runGameInteract game playRunner vis setup . runCryptoRNG gen . chooseRandom . logStdOut DebugLevel $ playGivenNodes [pure nodes]

runEffNodesAgainstState ::
  (Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l, Show r, Show pl, Show i, Eq ph) =>
  GameState l cn r ph pl i ->
  PlayRunner l cn r ph pl i ->
  VisibilityMap l cn ->
  Eff '[GameInteract 'Observe l cn r ph pl i] [GameNode l cn r ph pl i] ->
  [ Eff
      '[ Log2,
         Choosing,
         RNG,
         GameInteract 'Modify l cn r ph pl i,
         IOE
       ]
      [GameNode l cn r ph pl i]
  ] ->
  IO (GameState l cn r ph pl i)
runEffNodesAgainstState game playRunner vis setup nodes = do
  gen <- newCryptoRNGState
  runEff . runGameInteract game playRunner vis setup . runCryptoRNG gen . chooseRandom . logStdOut DebugLevel $ playGivenNodes nodes

instance Num a => Num (Eff es a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = return . fromInteger
  negate = fmap negate

-- data GameController l r cn r ph pl i m = GameController {
--     game :: Game l cn r ph pl i,

