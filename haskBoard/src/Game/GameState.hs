{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}


{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
module Game.GameState where
import Game.Player
import qualified Data.List.NonEmpty as NE
import GHC.Generics
import Data.Set (Set)
import Game.Location
import Game.GameNode
import GHC.Base (NonEmpty)
import Data.Map (Map)
import Data.Text ( Text, Text )
import Game.Visibility ( VisibilityMap, VisibilityMap )
import Control.Lens
    ( makeFields,
      Lens',
      ASetter,
      Getting,
      over,
      set,
      view,
      Ixed(..),
      At(..),
      (^.) )
import Count (Cnt)
import FinitaryMap (ftAt)
import Effectful.Dispatch.Static (StaticRep, SideEffects (..), getStaticRep, evalStaticRep, putStaticRep)
import Effectful (DispatchOf, Dispatch (..), Effect, Eff, (:>), raise, runPureEff)
import qualified Data.Text as T
import Effectful.Dispatch.Dynamic (send, interpret)
import Control.Applicative

data Turn phaseName = Turn {owner :: Player,
    turnPhases :: NE.NonEmpty phaseName} deriving (Eq, Ord, Show, Generic)

data Phase phaseName l cn r playName i = Phase
  { name :: phaseName,
    seedNodes :: GameState l cn r phaseName playName i -> [GameNode l cn r phaseName playName i]
  }
  deriving (Generic)

getPhaseNodes :: Phase phaseName l cn r playName i -> (GameState l cn r phaseName playName i -> [GameNode l cn r phaseName playName i])
getPhaseNodes (Phase _ seedNodes') = seedNodes'

type PlayRunner l cn r ph pl i = GameState l cn r ph pl i -> pl ->  [GameNode l cn r ph pl i]

data GameState l cn r ph pl i = GameState
  { players :: Set Player,
    objects :: GameObjects l cn r,
    currentPhase :: ph,
    phases :: ph -> Phase ph l cn r pl i,
    turns :: NonEmpty (Turn ph),
    currentTurn :: Turn ph,
    nextTurn :: Turn ph -> NonEmpty (Turn ph) -> Turn ph,
    displayHints :: Map Text Text
  }
  deriving (Generic)


data Game l cn r ph pl i = Game
  { gameState :: GameState l cn r ph pl i,
    playRunner :: PlayRunner l cn r ph pl i,
    visibility :: VisibilityMap l cn,
    setup :: GameState l cn r ph pl i -> [GameNode l cn r ph pl i]
  }
  deriving (Generic)

makeFields ''GameState
makeFields ''Game
makeFields ''Phase


counter :: Eq cn => cn -> Lens' (GameState l cn r ph pl i) Counter
counter c = #objects . #counters . ftAt c

counterVal :: Eq cn => cn -> Lens' (GameState l cn r ph pl i) (Cnt Int)
counterVal c = counter c . #val

location :: Eq l => l -> Lens' (GameState l cn r ph pl i) (LocationShape r)
location l = #objects . #locations . ftAt l


data Mode = Observe | Modify | ObserveAs Player deriving (Eq, Ord, Show, Generic)


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
  (GameState l cn r ph pl i -> [GameNode l cn r ph pl i]) ->
  Eff (GameInteract mode l cn r ph pl i : es) a ->
  Eff es a
runGameInteract gd pr vis setup = evalStaticRep (GameInteract (Game gd pr vis setup) :: StaticRep (GameInteract mode l cn r ph pl i))

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

getsSetup :: forall mode l cn r ph pl i es b. (GameInteract mode l cn r ph pl i :> es) => ((GameState l cn r ph pl i -> [GameNode l cn r ph pl i]) -> b) -> Eff es b
getsSetup f = do
  GameInteract (Game _ _ _ setup) <- getStaticRep @(GameInteract mode l cn r ph pl i)
  return (f setup)

getSetup :: forall mode l cn r ph pl i es. (GameInteract mode l cn r ph pl i :> es) => Eff es (GameState l cn r ph pl i -> [GameNode l cn r ph pl i])
getSetup = getsSetup id

hint :: forall mode a b l cn r ph pl i es. (GameInteract mode l cn r ph pl i :> es, Show a, Show b) => a -> b -> Eff es ()
hint name' content' = do
  let name = T.pack (show name')
  let content = T.pack (show content')
  GameInteract g <- getStaticRep @(GameInteract mode l cn r ph pl i)
  let g' = set (#gameState . #displayHints . ix name) content g
  putStaticRep (GameInteract g')

getHint :: (GameInteract mode l cn r ph pl i :> es) => Text -> Eff es (Maybe Text)
getHint name = useGameState (#displayHints . at name)


observe :: Game l cn r ph pl i -> Eff '[GameInteract mode l cn r ph pl i] a -> a
observe g = runPureEff . runGameInteract (g ^. #gameState) (g ^. #playRunner) (g ^. #visibility) (g^. #setup)

data BroadcastState l cn r ph pl i :: Effect where
    BroadcastState :: GameState l cn r ph pl i -> (BroadcastState l cn r ph pl i) m ()

type instance DispatchOf (BroadcastState l cn r ph pl i) = 'Dynamic

broadcastState :: forall l cn r ph pl mode es i. (BroadcastState l cn r ph pl i :> es, GameInteract mode l cn r ph pl i :> es) => Eff es ()
broadcastState = getGameState >>= send . BroadcastState

broadcastHandlerDummy :: Eff (BroadcastState l cn r ph pl i : es) a -> Eff es a
broadcastHandlerDummy = interpret $ \_ -> \case
    BroadcastState gs -> pure ()

instance Num a => Num (Eff es a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = return . fromInteger
  negate = fmap negate

