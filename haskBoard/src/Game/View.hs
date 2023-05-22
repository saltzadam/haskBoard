{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Game.View where
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Game.Player (Player)
import Game.Location (GameObjects, LocationShape, Counter)
import GHC.Generics (Generic)
import Game.Visibility (VisibilityMap (..), VisibilityType (..), runVis)
import FinitaryMap (FTMap (..), (!!!))
import Control.Lens ((^.), to)
import Control.Lens.TH (makeFields)
import Game.GameState

data GameStateViewC l cn r ph pl i = GameStateViewC
    {   playersViewC :: Set Player,
        objectsViewC :: GameObjectsViewC l cn r,
        currentPhaseViewC :: ph,
        phasesViewC :: ph -> Phase ph l cn r pl i,
        turnsViewC :: NonEmpty (Turn ph),
        currentTurnViewC :: Turn ph,
        nextTurnViewC :: Turn ph -> NonEmpty (Turn ph) -> Turn ph,
        visibilityC :: VisibilityMap l cn
    } deriving Generic


type LocationsViewC l r = FTMap l (Maybe (LocationShape r))
type CountersViewC cn = FTMap cn (Maybe Counter)

buildView' :: Maybe Player -> GameState l cn r ph pl i -> GameObjectsViewC l cn r  
buildView' (Just p) gs = let
    VisibilityMap vis = gs ^. #visibility
    locs = gs ^. #objects . #locations
    counters = gs ^. #objects . #counters
    locView l = runVis (vis p (Left l)) (locs !!! l)
    cView cn = runVis (vis p (Right cn)) (counters !!! cn)
    in GameObjectsViewC (FTMap locView) (FTMap cView)
buildView' Nothing gs = GameObjectsViewC 
                        (gs ^. #objects . #locations . to (fmap Just))
                        (gs ^. #objects . #counters .  to (fmap Just))


data GameObjectsViewC l cn r = GameObjectsViewC {
    locationsViewC :: LocationsViewC l r,
    countersViewC :: CountersViewC cn} deriving (Generic, Show)

makeFields ''GameStateViewC
makeFields ''GameObjectsViewC

viewObjectsAs' :: GameObjects l cn r -> VisibilityMap l cn -> Player -> GameObjectsViewC l cn r
viewObjectsAs' objs (VisibilityMap vis') p = let
    locs = objs ^. #locations
    locsVisMap = vis' p . Left
    locsViewC = FTMap (runVis <$> locsVisMap <*> (locs !!!))
    cns = objs ^. #counters
    cnsVisMap = vis' p . Right
    cnsViewC = FTMap (runVis <$> cnsVisMap <*> (cns !!!))
 in GameObjectsViewC locsViewC cnsViewC

viewGameStateAs' :: GameState l cn r ph pl i -> Player -> GameStateViewC l cn r ph pl i
viewGameStateAs' gs@(GameState ps _ cphase phss trns currTrn nextTrn vis) p =
    GameStateViewC ps
        (buildView' (Just p) gs)
        cphase phss trns currTrn nextTrn vis

-- viewGameAs' :: Game l cn r ph pl i -> Player -> GameStateViewC l cn r ph pl i
-- viewGameAs' (Game gs _ _) = viewGameStateAs' gs

