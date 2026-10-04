{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.View where

import Control.Lens (to, (^.), view)
import Control.Lens.TH (makeFields)
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, Value, decodeStrict, object, withObject, (.:), (.=))
import Game.Constraints (GameCounter, GameLocation, GamePhase, GameResource)
import Data.Finitary (Finitary, inhabitants)
import Data.Text (Text, pack)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Text.Encoding as T
import FinitaryMap (FTMap (..), ftAt, (!!!))
import GHC.Generics (Generic)
import Game.GameState
import Data.Map (Map)
import Game.Location (Counter (Counter), GameObjects (..), GymSpace (..), LocationShape (..), counterSpace, decodeLocationShape, encodeCounter, encodeLocationShape, locationShapeSpace, fromGymShape, toGymShape)
import Game.Player (Player, Turn (..))
import Game.Visibility (LookerType (..), VisData (..), VisibilityMap (..), runVis, allInvisible)

data GameStateView l cn r ph = GameStateView
  { playersView :: Set Player,
    objectsView :: GameObjectsView l cn r,
    currentPhaseView :: ph,
    currentTurnView :: Turn ph,
    nextTurnView :: Turn ph
    -- visibilityMapView :: VisibilityMap l cn r  -- this is problematic -- even knowing the keys of this map is too much info
  }
  deriving (Generic)

project :: GameState l cn r ph pl -> GameStateView l cn r ph
project gs =
  GameStateView
    { playersView = gs ^. #players,
      objectsView = buildView' Nothing gs,
      currentPhaseView = gs ^. #currentPhase,
      currentTurnView = gs ^. #currentTurn,
      nextTurnView = gs ^. #nextTurn
    }

inject :: GameStateView l cn r ph -> GameState l cn r ph pl
inject gsv =
  GameState {
      players = gsv ^. #playersView,
      objects = injectObjects (gsv ^. #objectsView),
      currentPhase = gsv ^. #currentPhaseView,
      currentTurn = gsv ^. #currentTurnView,
      nextTurn = gsv ^. #nextTurnView,
      visibility = allInvisible -- TODO: not great!
    }
      where
      injectObjects :: GameObjectsView l cn r -> GameObjects l cn r 
      injectObjects (GameObjectsView locView cntView) = GameObjects (injectLocs locView) (injectCnt cntView)

injectLocs :: FTMap a (Maybe (LocationShape r)) -> FTMap a (LocationShape r)
injectLocs (FTMap f) = FTMap (\x -> case f x of Just y -> y ; _ -> Dummy)
injectCnt (FTMap f) = FTMap (\x -> case f x of Just y -> y ; _ -> Counter 0 (0,0)) -- TODO: not great!

type LocationsView l r = FTMap l (Maybe (LocationShape r))

g :: forall  l r. Eq l => l -> LocationsView l r -> Maybe (LocationShape r)
g l' s =  view (ftAt l') (s :: LocationsView l r)

type CountersView cn = FTMap cn (Maybe Counter)

encodeLocationsView :: (Finitary r, Finitary l, ToJSON r, ToJSON l, Ord l, ToJSONKey l) => LocationsView l r -> Value
encodeLocationsView = toJSON . fmap (fmap (toGymShape . encodeLocationShape))

decodeLocationsView :: (Ord l, Ord r, Finitary l, Finitary r, FromJSONKey l) => Text -> LocationsView l r
decodeLocationsView = fmap (fmap (decodeLocationShape . fromGymShape)) . (fromJust . decodeStrict . T.encodeUtf8)

encodeCountersView :: (Finitary cn, ToJSON cn, Ord cn, ToJSONKey cn) => CountersView cn -> Value
encodeCountersView = toJSON . fmap (fmap encodeCounter)

data GameObjectsView l cn r = GameObjectsView
  { locationsView :: LocationsView l r,
    countersView :: CountersView cn
  }
  deriving (Generic, Show)

viewObjectsAs' :: GameObjects l cn r -> VisibilityMap l cn -> Player -> GameObjectsView l cn r
viewObjectsAs' objs (VisibilityMap vis') p =
  let locs = objs ^. #locations
      locsVisMap = vis' p . VisLocation
      locsView = runVis <$> locsVisMap <*> (locs !!!)
      cns = objs ^. #counters
      cnsVisMap = vis' p . VisCounter
      cnsView = runVis <$> cnsVisMap <*> (cns !!!)
   in GameObjectsView (FTMap locsView) (FTMap cnsView)

buildView' :: Maybe Player -> GameState l cn r ph pl -> GameObjectsView l cn r
buildView' (Just p) gs = viewObjectsAs' (gs ^. #objects) (gs ^. #visibility) p
buildView' Nothing gs =
  GameObjectsView
    (gs ^. #objects . #locations . to (fmap Just))
    (gs ^. #objects . #counters . to (fmap Just))

instance (GameLocation l, GameCounter cn, GameResource r) => ToJSON (GameObjectsView l cn r) where
  toJSON (GameObjectsView locs cts) =
    object
      [ "locations" .= encodeLocationsView locs,
        "counters" .= encodeCountersView cts
      ]

instance (GameLocation l, GameCounter cn, GameResource r) => FromJSON (GameObjectsView l cn r) where
  parseJSON = withObject "GameObjectsView" $ \v ->
    GameObjectsView
      <$> v .: "locations"
      <*> v .: "counters"

deriving instance (GameLocation l, GameCounter cn, GameResource r, GamePhase ph) => FromJSON (GameStateView l cn r ph)

deriving instance (GameLocation l, GameCounter cn, GameResource r, GamePhase ph) => ToJSON (GameStateView l cn r ph)

viewGameStateAs' :: GameState l cn r ph pl -> Player -> GameStateView l cn r ph
viewGameStateAs' gs p =
  GameStateView
    (gs ^. #players)
    (buildView' (Just p) gs)
    (gs ^. #currentPhase)
    (gs ^. #currentTurn)
    (gs ^. #nextTurn)

viewGameStateAs :: GameState l cn r ph pl -> LookerType -> GameStateView l cn r ph
viewGameStateAs gs (LookAs p) = viewGameStateAs' gs p
viewGameStateAs gs LookFull = project gs

makeFields ''GameStateView
makeFields ''GameObjectsView

-- | Derive a GymSpace descriptor from a player's view of the game objects.
-- Hidden locations/counters (Nothing in the view) are omitted entirely.
-- The @totals@ map (from 'inventoryTotals') determines binary encoding widths.
gameObjectsViewSpace
  :: forall l cn r. (GameLocation l, GameCounter cn, GameResource r)
  => Map r Int -> GameObjectsView l cn r -> GymSpace
gameObjectsViewSpace totals (GameObjectsView locsView cnsView) = GymDict $
  [ (pack (show l), locationShapeSpace totals loc)
  | l <- inhabitants @l, Just loc <- [locsView !!! l] ]
  ++
  [ (pack (show cn), counterSpace c)
  | cn <- inhabitants @cn, Just c <- [cnsView !!! cn] ]


