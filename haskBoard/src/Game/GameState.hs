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
import Game.Visibility ( VisibilityMap, VisibilityMap )
import Control.Lens
    ( makeFields,
      Lens',
      ASetter,
      Getting,
      over,
      view,
      to )
import Count (Cnt)
import FinitaryMap (ftAt)
import Effectful (DispatchOf, Dispatch (..), Effect, Eff, (:>))
import Effectful.Dispatch.Dynamic (send, interpret)
import qualified Effectful.State.Static.Shared as State

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
    visibility :: VisibilityMap l cn
  }
  deriving (Generic)


data Game l cn r ph pl i = Game
  { gameState :: GameState l cn r ph pl i,
    playRunner :: PlayRunner l cn r ph pl i,
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

type GameInteract l cn r ph pl i = State.State (Game l cn r ph pl i)

type ObserveGame l cn r ph pl i es = GameInteract l cn r ph pl i :> es


useGame :: (GameInteract l cn r ph pl i :> es) => Getting b (Game l cn r ph pl i) b -> Eff es b
useGame o = State.gets (view o)

getsGameState :: (GameInteract l cn r ph pl i :> es) =>  (GameState l cn r ph pl i -> b) -> Eff es b
getsGameState f = State.gets (view (#gameState . to f))

getGameState :: (GameInteract l cn r ph pl i :> es) => Eff es (GameState l cn r ph pl i)
getGameState = getsGameState id

useGameState :: (GameInteract l cn r ph pl i :> es) => Getting b (GameState l cn r ph pl i) b -> Eff es b
useGameState o = getsGameState (view o)


getRunner :: (GameInteract l cn r ph pl i :> es) => Eff es (GameState l cn r ph pl i
                  -> pl -> [GameNode l cn r ph pl i])
getRunner = useGame #playRunner

modifyingGame :: (GameInteract l cn r ph pl i :> es) => ASetter (Game l cn r ph pl i) (Game l cn r ph pl i) a b -> (a -> b) -> Eff es ()
modifyingGame o = State.modify . over o

modifyingGameState :: (GameInteract l cn r ph pl i :> es) => ASetter  (GameState l cn r ph pl i) (GameState l cn r ph pl i) a b -> (a -> b) -> Eff es ()
modifyingGameState o = State.modify . over (#gameState . o)

assignGameState :: (GameInteract l cn r ph pl i :> es) => ASetter  (GameState l cn r ph pl i) (GameState l cn r ph pl i) a b -> b -> Eff es ()
assignGameState l b = modifyingGameState l (const b)

getVisibility :: (GameInteract l cn r ph pl i :> es) => Eff es (VisibilityMap l cn)
getVisibility = useGame (#gameState . #visibility)

modifyVisibility :: (GameInteract l cn r ph pl i :> es) => (VisibilityMap l cn -> VisibilityMap l cn) -> Eff es ()
modifyVisibility = modifyingGame (#gameState . #visibility)

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

