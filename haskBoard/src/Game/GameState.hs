{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Game.GameState
  ( module Game.GameStateBase,
    PhaseControl (..),
    TurnControl (..),
    Phase (..),
    PlayRunner,
    GameRules (..),
    GameInteract,
    GameRun,
    counter,
    counterVal,
    location,
    getsGameState,
    getGameState,
    useGameState,
    getRunner,
    getPhases,
    getScore,
    getSetupPhase,
    modifyingGameState,
    assignGameState,
    getVisibility,
    modifyVisibility,
  )
where

import Control.Lens
  ( ASetter,
    Getting,
    Lens',
    makeFields,
    over,
    view,
  )
import Effectful (Eff, (:>))
import qualified Effectful.Reader.Static as Reader
import qualified Effectful.State.Static.Shared as State
import FinitaryMap (ftAt)
import GHC.Generics (Generic)
import Game.GameStateBase (GameState (..))
import Game.Location
import Game.Player
import Game.Rules (GameRule)
import Game.Visibility (VisibilityMap (..))

data PhaseControl = PCContinue | PCEndPhase | PCEndTurn | PCEndGame [Player] deriving (Eq, Ord, Show, Generic)

data TurnControl = TEndTurn | TEndGame [Player] deriving (Eq, Ord, Show, Generic)

data Phase phaseName l cn r playName = Phase
  { name :: phaseName,
    seedNodes :: GameRule l cn r phaseName playName ()
  }
  deriving (Generic)

type PlayRunner l cn r ph pl = pl -> GameRule l cn r ph pl ()

data GameRules l cn r ph pl = GameRules
  { playRunner :: PlayRunner l cn r ph pl,
    phases :: ph -> Phase ph l cn r pl,
    score :: Player -> GameRule l cn r ph pl Int,
    setupPhase :: Maybe ph
  }
  deriving (Generic)

-- These lenses basically exist for GameE
-- They shouldn't be used for writing games.
counter :: (Eq cn) => cn -> Lens' (GameState l cn r ph pl) Counter
counter c = #objects . #counters . ftAt c

counterVal :: (Eq cn) => cn -> Lens' (GameState l cn r ph pl) Int
counterVal c = counter c . #val

location :: (Eq l) => l -> Lens' (GameState l cn r ph pl) (LocationShape r)
location l = #objects . #locations . ftAt l

type GameInteract l cn r ph pl = State.State (GameState l cn r ph pl)

-- TODO: package this and eliminate Game
type GameRun l cn r ph pl = Reader.Reader (GameRules l cn r ph pl)

makeFields ''GameState
makeFields ''GameRules
makeFields ''Phase

getsGameState :: (GameInteract l cn r ph pl :> es) => (GameState l cn r ph pl -> b) -> Eff es b
getsGameState = State.gets

getGameState :: (GameInteract l cn r ph pl :> es) => Eff es (GameState l cn r ph pl)
getGameState = getsGameState id

useGameState :: (GameInteract l cn r ph pl :> es) => Getting b (GameState l cn r ph pl) b -> Eff es b
useGameState o = getsGameState (view o)

getRunner :: (GameRun l cn r ph pl :> es) => Eff es (PlayRunner l cn r ph pl)
getRunner = Reader.asks (view #playRunner)

getPhases :: (GameRun l cn r ph pl :> es) => Eff es (ph -> Phase ph l cn r pl)
getPhases = Reader.asks (view #phases)

getScore :: (GameRun l cn r ph pl :> es) => Eff es (Player -> GameRule l cn r ph pl Int)
getScore = Reader.asks (view #score)

getSetupPhase :: (GameRun l cn r ph pl :> es) => Eff es (Maybe ph)
getSetupPhase = Reader.asks (view #setupPhase)

modifyingGameState :: (GameInteract l cn r ph pl :> es) => ASetter (GameState l cn r ph pl) (GameState l cn r ph pl) a b -> (a -> b) -> Eff es ()
modifyingGameState o = State.modify . over o

assignGameState :: (GameInteract l cn r ph pl :> es) => ASetter (GameState l cn r ph pl) (GameState l cn r ph pl) a b -> b -> Eff es ()
assignGameState l b = modifyingGameState l (const b)

getVisibility :: (GameInteract l cn r ph pl :> es) => Eff es (VisibilityMap l cn)
getVisibility = useGameState #visibility

modifyVisibility :: (GameInteract l cn r ph pl :> es) => (VisibilityMap l cn -> VisibilityMap l cn) -> Eff es ()
modifyVisibility = modifyingGameState #visibility
