{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module View where
import GameE (Turn, Phase, GameState (..), Game (..),getGameState, GameInteract, PlayRunner, Mode (..), getVisibility)
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Game.Player (Player)
import Location (GameObjects, LocationShape, Counter)
import GHC.Generics (Generic)
import Visibility (VisibilityMap (..), VisibilityType (..))
import FinitaryMap (FTMap (..), ftAt)
import Control.Lens ((^.), Lens')
import Effectful (Eff, (:>))
import Control.Lens.TH (makeFields)
import GameNode (GameNode)
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M


data GameStateView l cn r ph pl i = GameStateView
    {   playersView :: Set Player,
        objectsView :: GameObjectsView l cn r,
        currentPhaseView :: ph,
        phasesView :: ph -> Phase ph l cn r pl i,
        turnsView :: NonEmpty (Turn ph),
        currentTurnView :: Turn ph,
        nextTurnView :: Turn ph -> NonEmpty (Turn ph) -> Turn ph,
        displayHintsView :: Map Text Text
    } deriving Generic


type LocationsView l r = FTMap l (Maybe (LocationShape r))
type CountersView cn = FTMap cn (Maybe Counter)


locationView :: Eq l => l -> Lens' (GameStateView l cn r ph pl i) (Maybe (LocationShape r))
locationView l = #objectsView . #locationsView . ftAt l



runVis :: VisibilityType -> a -> Maybe a
runVis Invisible _ = Nothing
runVis Visible a = Just a

data GameObjectsView l cn r = GameObjectsView {
    locationsView :: LocationsView l r,
    countersView :: CountersView cn} deriving (Generic, Show)

data GameView l cn r ph pl i = GameView
    { gameStateView :: GameStateView l cn r ph pl i,
      playRunnerView :: PlayRunner l cn r ph pl i,
      visibilityView :: VisibilityMap l cn,
      setupView :: Eff '[GameInteract 'Observe l cn r ph pl i] [GameNode l cn r ph pl i]
    } deriving (Generic)

makeFields ''GameStateView
makeFields ''GameObjectsView
makeFields ''GameView

getHintView :: Text -> GameStateView l cn r ph pl i -> Maybe Text
getHintView name = M.lookup name . displayHintsView

viewObjectsAs' :: GameObjects l cn r -> VisibilityMap l cn -> Player -> GameObjectsView l cn r
viewObjectsAs' objs vis p = let
    locs = objs ^. #locations
    locsVisMap = canSee vis p . Left
    locsView = FTMap (runVis <$> locsVisMap <*> runFn locs)
    cns = objs ^. #counters
    cnsVisMap = canSee vis p . Right
    cnsView = FTMap (runVis <$> cnsVisMap <*> runFn cns)
 in GameObjectsView locsView cnsView

viewGameStateAs' :: GameState l cn r ph pl i -> VisibilityMap l cn -> Player -> GameStateView l cn r ph pl i
viewGameStateAs' (GameState ps objs cphase phss trns currTrn nextTrn hints) vis p =
    GameStateView ps
        (viewObjectsAs' objs vis p)
        cphase phss trns currTrn nextTrn hints

viewGameAs' :: Game l cn r ph pl i -> Player -> GameStateView l cn r ph pl i
viewGameAs' (Game gs _ vis _) = viewGameStateAs' gs vis

viewGameAs :: GameInteract mode l cn r ph pl i :> es => Player -> Eff es (GameStateView l cn r ph pl i)
viewGameAs p = do
    vis <- getVisibility
    gs <- getGameState
    return $ viewGameStateAs' gs vis p


