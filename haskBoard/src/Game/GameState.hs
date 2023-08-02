{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Game.GameState where

import Control.Lens
  ( ASetter,
    Getting,
    Lens',
    makeFields,
    over,
    view,
  )
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import Effectful (Eff, (:>))
import qualified Effectful.Reader.Static as Reader
import qualified Effectful.State.Static.Shared as State
import FinitaryMap (ftAt)
import GHC.Generics (Generic)
import Game.Location
import Game.Player
import Game.Rules (GameRule)
import Game.Visibility (VisibilityMap (..))

data Turn phaseName = Turn
  { owner :: Player,
    turnPhases :: NE.NonEmpty phaseName
  }
  deriving (Eq, Ord, Show, Generic)

data PhaseControl = PCContinue | PCEndPhase | PCEndTurn | PCEndGame deriving (Eq, Ord, Show, Generic)

data TurnControl = TEndTurn | TEndGame deriving (Eq, Ord, Show, Generic)

data Phase phaseName l cn r playName i = Phase
  { name :: phaseName,
    seedNodes :: [GameRule l cn r phaseName playName i ()]
  }
  deriving (Generic)

type PlayRunner l cn r ph pl i = pl -> GameRule l cn r ph pl i ()

data GameState l cn r ph pl i = GameState
  { players :: Set Player,
    objects :: GameObjects l cn r,
    currentPhase :: ph,
    -- owner :: l -> Maybe Player,
    currentTurn :: Turn ph,
    nextTurn :: GameState l cn r ph pl i -> Turn ph,
    visibility :: VisibilityMap l cn ph
  }
  deriving (Generic)

data GameRules l cn r ph pl i = GameRules
  { playRunner :: PlayRunner l cn r ph pl i,
    phases :: ph -> Phase ph l cn r pl i,
    score :: Player -> GameRule l cn r ph pl i Int,
    setupPhase :: Maybe ph
  }
  deriving (Generic)

-- These lenses basically exist for GameE
-- They shouldn't be used for writing games.
counter :: Eq cn => cn -> Lens' (GameState l cn r ph pl i) Counter
counter c = #objects . #counters . ftAt c

counterVal :: Eq cn => cn -> Lens' (GameState l cn r ph pl i) Int
counterVal c = counter c . #val

location :: Eq l => l -> Lens' (GameState l cn r ph pl i) (LocationShape r)
location l = #objects . #locations . ftAt l

type GameInteract l cn r ph pl i = State.State (GameState l cn r ph pl i)

-- TODO: package this and eliminate Game
type GameRun l cn r ph pl i = Reader.Reader (GameRules l cn r ph pl i)

makeFields ''GameState
makeFields ''GameRules
makeFields ''Phase

getsGameState :: (GameInteract l cn r ph pl i :> es) => (GameState l cn r ph pl i -> b) -> Eff es b
getsGameState = State.gets

getGameState :: (GameInteract l cn r ph pl i :> es) => Eff es (GameState l cn r ph pl i)
getGameState = getsGameState id

useGameState :: (GameInteract l cn r ph pl i :> es) => Getting b (GameState l cn r ph pl i) b -> Eff es b
useGameState o = getsGameState (view o)

getRunner :: (GameRun l cn r ph pl i :> es) => Eff es (PlayRunner l cn r ph pl i)
getRunner = Reader.asks (view #playRunner)

getPhases :: (GameRun l cn r ph pl i :> es) => Eff es (ph -> Phase ph l cn r pl i)
getPhases = Reader.asks (view #phases)

getScore :: (GameRun l cn r ph pl i :> es) => Eff es (Player -> GameRule l cn r ph pl i Int)
getScore = Reader.asks (view #score)

getSetupPhase :: (GameRun l cn r ph pl i :> es) => Eff es (Maybe ph)
getSetupPhase = Reader.asks (view #setupPhase)

modifyingGame :: (GameInteract l cn r ph pl i :> es) => ASetter (GameState l cn r ph pl i) (GameState l cn r ph pl i) a b -> (a -> b) -> Eff es ()
modifyingGame o = State.modify . over o

modifyingGameState :: (GameInteract l cn r ph pl i :> es) => ASetter (GameState l cn r ph pl i) (GameState l cn r ph pl i) a b -> (a -> b) -> Eff es ()
modifyingGameState o = State.modify . over o

assignGameState :: (GameInteract l cn r ph pl i :> es) => ASetter (GameState l cn r ph pl i) (GameState l cn r ph pl i) a b -> b -> Eff es ()
assignGameState l b = modifyingGameState l (const b)

getVisibility :: (GameInteract l cn r ph pl i :> es) => Eff es (VisibilityMap l cn ph)
getVisibility = useGameState #visibility

modifyVisibility :: (GameInteract l cn r ph pl i :> es) => (VisibilityMap l cn ph -> VisibilityMap l cn ph) -> Eff es ()
modifyVisibility = modifyingGame #visibility
