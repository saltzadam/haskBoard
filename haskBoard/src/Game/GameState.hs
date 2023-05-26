{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
module Game.GameState where
import Game.Player
import qualified Data.List.NonEmpty as NE
import GHC.Generics (Generic)
import Data.Set (Set)
import Game.Location
import Game.GameNode
import GHC.Base (NonEmpty)
import Game.Visibility ( VisibilityMap (..), VisibilityMap, runVis, VisData (..) )
import Control.Lens
    ( makeFields,
      Lens',
      ASetter,
      Getting,
      over,
      view,
      to, lens, Getter, (^.) )
import Count (Cnt)
import FinitaryMap (ftAt)
import Effectful (DispatchOf, Dispatch (..), Effect, Eff, (:>))
import Effectful.Dispatch.Dynamic (send, interpret)
import qualified Effectful.State.Static.Shared as State
import Data.Finitary (Finitary)
import qualified Effectful.Reader.Static as Reader

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
    visibility :: VisibilityMap l cn ph
  }
  deriving (Generic)

data GameStateShow l cn r ph = GameStateShow
    { players :: Set Player,
      objects :: GameObjects l cn r,
      currentPhase :: ph
    } deriving (Generic, Show)

projectShow :: GameState l cn r ph pl i -> GameStateShow l cn r ph
projectShow (GameState pl obj curr _ _ _ _ _) = GameStateShow pl obj curr

instance (Finitary l, Finitary cn, Show l, Show r, Show cn, Show ph) => Show (GameState l cn r ph pl i) where
    show = show . projectShow

data Game l cn r ph pl i = Game
  { gameState :: GameState l cn r ph pl i,
    playRunner :: PlayRunner l cn r ph pl i,
    setup :: Int -> [GameNode l cn r ph pl i]
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

type GameInteract l cn r ph pl i = State.State (GameState l cn r ph pl i)
type GameRun l cn r ph pl i = Reader.Reader (PlayRunner l cn r ph pl i, Int -> [GameNode l cn r ph pl i])

type ObserveGame l cn r ph pl i es = GameInteract l cn r ph pl i :> es


getsGameState :: (GameInteract l cn r ph pl i :> es) =>  (GameState l cn r ph pl i -> b) -> Eff es b
getsGameState = State.gets

getGameState :: (GameInteract l cn r ph pl i :> es) => Eff es (GameState l cn r ph pl i)
getGameState = getsGameState id

useGameState :: (GameInteract l cn r ph pl i :> es) => Getting b (GameState l cn r ph pl i) b -> Eff es b
useGameState o = getsGameState (view o)


getRunner :: (GameRun l cn r ph pl i :> es) => Eff es (GameState l cn r ph pl i
                  -> pl -> [GameNode l cn r ph pl i])
getRunner = fst <$> Reader.ask 

getSetup :: (GameRun l cn r ph pl i :> es) => Eff es (Int -> [GameNode l cn r ph pl i])
getSetup = snd <$> Reader.ask

modifyingGame :: (GameInteract l cn r ph pl i :> es) => ASetter (GameState l cn r ph pl i) (GameState l cn r ph pl i) a b -> (a -> b) -> Eff es ()
modifyingGame o = State.modify . over o

modifyingGameState :: (GameInteract l cn r ph pl i :> es) => ASetter  (GameState l cn r ph pl i) (GameState l cn r ph pl i) a b -> (a -> b) -> Eff es ()
modifyingGameState o = State.modify . over o

assignGameState :: (GameInteract l cn r ph pl i :> es) => ASetter  (GameState l cn r ph pl i) (GameState l cn r ph pl i) a b -> b -> Eff es ()
assignGameState l b = modifyingGameState l (const b)

getVisibility :: (GameInteract l cn r ph pl i :> es) => Eff es (VisibilityMap l cn ph)
getVisibility = useGameState #visibility

modifyVisibility :: (GameInteract l cn r ph pl i :> es) => (VisibilityMap l cn ph -> VisibilityMap l cn ph) -> Eff es ()
modifyVisibility = modifyingGame #visibility

data BroadcastState l cn r ph pl i :: Effect where
    BroadcastState :: GameState l cn r ph pl i -> (BroadcastState l cn r ph pl i) m ()

type instance DispatchOf (BroadcastState l cn r ph pl i) = 'Dynamic

broadcastState :: forall l cn r ph pl es i. (BroadcastState l cn r ph pl i :> es, GameInteract l cn r ph pl i :> es) => Eff es ()
broadcastState = getGameState >>= send . BroadcastState

broadcastHandlerDummy :: Eff (BroadcastState l cn r ph pl i : es) a -> Eff es a
broadcastHandlerDummy = interpret $ \_ -> \case
    BroadcastState _ -> pure ()


-- instance Num a => Num (Eff es a) where
--   (+) = liftA2 (+)
--   (*) = liftA2 (*)
--   abs = fmap abs
--   signum = fmap signum
--   fromInteger = return . fromInteger
--   negate = fmap negate

