{-# LANGUAGE TemplateHaskell #-}
module Game.View where
import Data.Set (Set)
import Game.Player (Player)
import Game.Location (GameObjects, LocationShape, Counter)
import GHC.Generics (Generic)
import Game.Visibility (VisibilityMap (..), runVis, VisData (..))
import FinitaryMap (FTMap (..), (!!!))
import Control.Lens ((^.), to)
import Control.Lens.TH (makeFields)
import Game.GameState
import Game.Monad (LookerType(..))

data GameStateView l cn r ph = GameStateView
    {   playersView :: Set Player,
        objectsView :: GameObjectsView l cn r,
        currentPhaseView :: ph,
        currPlayer :: Player
    } deriving Generic

project :: GameState l cn r ph pl i -> GameStateView l cn r ph
project gs = GameStateView {playersView = gs ^. #players,
    objectsView = buildView' Nothing gs,
    currentPhaseView = gs ^. #currentPhase,
    currPlayer = gs ^. #currentTurn . #owner
                           }

type LocationsView l r = FTMap l (Maybe (LocationShape r))
type CountersView cn = FTMap cn (Maybe Counter)

buildView' :: Maybe Player -> GameState l cn r ph pl i -> GameObjectsView l cn r  
buildView' (Just p) gs = let
    VisibilityMap vis = gs ^. #visibility
    locs = gs ^. #objects . #locations
    counters = gs ^. #objects . #counters
    locView l = runVis (vis p (VisLocation l)) (locs !!! l)
    cView cn = runVis (vis p (VisCounter cn)) (counters !!! cn)
    in GameObjectsView (FTMap locView) (FTMap cView)
buildView' Nothing gs = GameObjectsView 
                        (gs ^. #objects . #locations . to (fmap Just))
                        (gs ^. #objects . #counters .  to (fmap Just))


data GameObjectsView l cn r = GameObjectsView {
    locationsView :: LocationsView l r,
    countersView :: CountersView cn} deriving (Generic)

makeFields ''GameStateView
makeFields ''GameObjectsView

viewObjectsAs' :: GameObjects l cn r -> VisibilityMap l cn ph -> Player -> GameObjectsView l cn r
viewObjectsAs' objs (VisibilityMap vis') p = let
    locs = objs ^. #locations
    locsVisMap = vis' p . VisLocation
    locsView = runVis <$> locsVisMap <*> (locs !!!)
    cns = objs ^. #counters
    cnsVisMap = vis' p . VisCounter
    cnsView = runVis <$> cnsVisMap <*> (cns !!!)
 in GameObjectsView (FTMap locsView) (FTMap cnsView)

viewGameStateAs' :: GameState l cn r ph pl i -> Player -> GameStateView l cn r ph
viewGameStateAs' gs@(GameState{players = ps, currentPhase = cphase, currentTurn = (Turn p' _)}) p =
    GameStateView ps
        (buildView' (Just p) gs)
        cphase p'

viewGameStateAs :: GameState l cn r ph pl i -> LookerType -> GameStateView l cn r ph
viewGameStateAs gs (LookAs p) = viewGameStateAs' gs p
viewGameStateAs gs LookFull = project gs

