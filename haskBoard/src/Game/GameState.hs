{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}
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
    counter,
    counterVal,
    location,
    scoresOf,
  )
where

import Control.Lens (Lens', makeFields, (^.))
import Data.Map (Map)
import qualified Data.Map as M
import FinitaryMap (ftAt)
import GHC.Generics (Generic)
import Game.GameStateBase (GameState (..))
import Game.Location
import Game.Player
import Game.Rules (GameRule, Query, runQuery)

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
    score :: Player -> Query l cn r ph pl Int,
    -- | (lo, hi) bounds used to describe the score observation space.
    scoreBounds :: (Int, Int),
    -- | When True, all players' scores are included in every agent's observation.
    -- When False, each agent sees only their own score.
    scorePublic :: Bool,
    -- | Optional one-shot setup logic run before the first turn.
    -- Stored as a 'GameRule' directly so the phase type 'ph' does not need
    -- a dedicated setup constructor.
    setupPhase :: Maybe (GameRule l cn r ph pl ())
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

makeFields ''GameState
makeFields ''GameRules
makeFields ''Phase

-- | Every player's current score.
scoresOf :: GameRules l cn r ph pl -> GameState l cn r ph pl -> Map Player Int
scoresOf gr gs = M.fromSet (\p -> runQuery ((gr ^. #score) p) gs) (gs ^. #players)
