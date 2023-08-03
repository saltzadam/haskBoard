{-# LANGUAGE TemplateHaskell #-}

module Game.View where

import Control.Lens (to, (^.))
import Control.Lens.TH (makeFields)
import Control.Monad.Free (Free (..))
import Data.Set (Set)
import Effectful (Eff, (:>))
import FinitaryMap (FTMap (..), ftAt, (!!!))
import GHC.Generics (Generic)
import Game.GameState
import Game.Location (Counter, GameObjects, LocationShape)
import Game.Options (Options)
import Game.Player (Player)
import Game.Rules
import Game.Visibility (LookerType (..), VisData (..), VisibilityMap (..), VisibilityType (..), runVis)

-- View is used for writing UIs and interfaces. The controller sends the interface a View which only contains information that is Visible to the user.
-- There are two ways to produce Views. One uses the GameRule monad. This should enable some code reuse: the same code that defines the game rules can also be used for views, and visibility will "just work." But how to run it? Interfaces currently demand GameStateView. They could instead send GameRules for interpretation. So this is still WIP.

-- The other way is to explicitly create a GameStateView object.

viewRule' :: (Eq l, GameInteract l cn r ph pl i :> es, Eq cn) => Player -> (Options pl i -> Eff es pl) -> GameRule l cn r ph pl i a -> Eff es (Maybe a)
viewRule' p c (Free (Act _ next)) = viewRule' p c next
viewRule' p c (Free (MakeChoice opts next)) = do
  pl <- c opts
  viewRule' p c (next pl)
viewRule' p c (Free (LookLocation l next)) =
  do
    shape <- useGameState (#objects . #locations . ftAt l)
    VisibilityMap canSee <- getVisibility
    case canSee p (VisLocation l) of
      Invisible -> return Nothing
      Visible -> viewRule' p c (next shape)
viewRule' p c (Free (LookCounter cn next)) = do
  shape <- useGameState (#objects . #counters . ftAt cn)
  VisibilityMap canSee <- getVisibility
  case canSee p (VisCounter cn) of
    Invisible -> return Nothing
    Visible -> viewRule' p c (next shape)
viewRule' p c (Free (LookCurrentPhase next)) = do
  phase <- useGameState #currentPhase
  VisibilityMap canSee <- getVisibility
  case canSee p VisCurrentPhase of
    Invisible -> return Nothing
    Visible -> viewRule' p c (next phase)
viewRule' p c (Free (LookCurrentTurnOwner next)) = do
  Turn currentPlayer _ <- useGameState #currentTurn
  VisibilityMap canSee <- getVisibility
  case canSee p (VisTurn currentPlayer) of
    Invisible -> return Nothing
    Visible -> viewRule' p c (next currentPlayer)
viewRule' p c (Free (LookPlayers next)) = do
  players <- useGameState #players
  viewRule' p c (next players)
viewRule' _ _ (Pure a) = return (Just a)

--- ====== ----

data GameStateView l cn r ph = GameStateView
  { playersView :: Set Player,
    objectsView :: GameObjectsView l cn r,
    currentPhaseView :: ph,
    currPlayer :: Player
  }
  deriving (Generic)

project :: GameState l cn r ph pl i -> GameStateView l cn r ph
project gs =
  GameStateView
    { playersView = gs ^. #players,
      objectsView = buildView' Nothing gs,
      currentPhaseView = gs ^. #currentPhase,
      currPlayer = gs ^. #currentTurn . #owner
    }

type LocationsView l r = FTMap l (Maybe (LocationShape r))

type CountersView cn = FTMap cn (Maybe Counter)

buildView' :: Maybe Player -> GameState l cn r ph pl i -> GameObjectsView l cn r
buildView' (Just p) gs =
  let VisibilityMap vis = gs ^. #visibility
      locs = gs ^. #objects . #locations
      counters = gs ^. #objects . #counters
      locView l = runVis (vis p (VisLocation l)) (locs !!! l)
      cView cn = runVis (vis p (VisCounter cn)) (counters !!! cn)
   in GameObjectsView (FTMap locView) (FTMap cView)
buildView' Nothing gs =
  GameObjectsView
    (gs ^. #objects . #locations . to (fmap Just))
    (gs ^. #objects . #counters . to (fmap Just))

data GameObjectsView l cn r = GameObjectsView
  { locationsView :: LocationsView l r,
    countersView :: CountersView cn
  }
  deriving (Generic)

makeFields ''GameStateView
makeFields ''GameObjectsView

viewObjectsAs' :: GameObjects l cn r -> VisibilityMap l cn ph -> Player -> GameObjectsView l cn r
viewObjectsAs' objs (VisibilityMap vis') p =
  let locs = objs ^. #locations
      locsVisMap = vis' p . VisLocation
      locsView = runVis <$> locsVisMap <*> (locs !!!)
      cns = objs ^. #counters
      cnsVisMap = vis' p . VisCounter
      cnsView = runVis <$> cnsVisMap <*> (cns !!!)
   in GameObjectsView (FTMap locsView) (FTMap cnsView)

viewGameStateAs' :: GameState l cn r ph pl i -> Player -> GameStateView l cn r ph
viewGameStateAs' gs@(GameState {players = ps, currentPhase = cphase, currentTurn = (Turn p' _)}) p =
  GameStateView
    ps
    (buildView' (Just p) gs)
    cphase
    p'

viewGameStateAs :: GameState l cn r ph pl i -> LookerType -> GameStateView l cn r ph
viewGameStateAs gs (LookAs p) = viewGameStateAs' gs p
viewGameStateAs gs LookFull = project gs
