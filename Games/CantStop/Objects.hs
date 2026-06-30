{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Objects where

import Data.Finitary (Finitary, inhabitants)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import FinitaryMap (FTMap (..))
import GHC.Generics (Generic)
import Game.GameAction
import Game.GameState
import Game.Location
import Game.Options (Options)
import Game.Player
import Game.Rules
import Game.View (GameStateView)
import Track (Track (..))
import NumberedPiece (NumberedPiece, number)

newtype TrackName = TrackName (NumberedPiece 11)
  deriving (Eq, Ord, Show, Generic, Finitary)

trackNames :: [TrackName]
trackNames = inhabitants 

trackNum :: TrackName -> Int
trackNum (TrackName i) = number i + 2

newtype TrackHeight = TrackHeight (NumberedPiece 13)
  deriving (Eq, Ord, Show, Generic, Finitary)

trackHeights :: [TrackHeight]
trackHeights = inhabitants

trackHeight :: TrackHeight -> Int
trackHeight (TrackHeight i) = number i + 1

data CantStopLocation
  = BoxTop
  | TrackSpot TrackName TrackHeight
  deriving (Eq, Ord, Show, Generic, Finitary)

maxSlot :: TrackName -> Int
maxSlot t =
  if trackNum t <= 7
    then trackNum t
    else 24 - trackNum t

track :: TrackName -> Track CantStopLocation
track name = Track (NE.fromList [TrackSpot name slot | slot <- inhabitants, trackHeight slot <= maxSlot name])

diceToTrack :: Int -> TrackName
diceToTrack x = TrackName (toEnum $ x - 2)

data CantStopResource = PlayerMarker Player | TemporaryMarker deriving (Eq, Ord, Show, Generic, Finitary)

data CantStopCounterName = DieOne | DieTwo | DieThree | DieFour
  deriving (Eq, Ord, Show, Generic, Finitary)

type CantStopLocations = Locations CantStopLocation CantStopResource

type CantStopCounters = Counters CantStopCounterName

type CantStopGameObjects = GameObjects CantStopLocation CantStopCounterName CantStopResource

theDice :: [CantStopCounterName]
theDice = inhabitants

marker :: Maybe Player -> CantStopResource
marker (Just p) = PlayerMarker p
marker Nothing = TemporaryMarker

owner :: CantStopResource -> Maybe Player
owner (PlayerMarker p) = Just p
owner TemporaryMarker = Nothing

initLocations' :: Set Player -> CantStopLocation -> LocationShape CantStopResource
initLocations' players BoxTop = Pile $ M.singleton TemporaryMarker 3 <> mconcat [M.singleton (PlayerMarker player) 11 | player <- S.toList players]
initLocations' _ _ = Pile M.empty

initLocations :: Set Player -> CantStopLocations
initLocations = FTMap . initLocations'

initCounters :: CantStopCounterName -> Counter
initCounters _ = d6

initGameObjects :: Set Player -> CantStopGameObjects
initGameObjects ps =
  GameObjects
    { locations = initLocations ps,
      counters = FTMap initCounters
    }

data CantStopPlayName
  = TwoMove TrackName TrackName
  | OneMove TrackName
  | Stop
  | DontStop
  | ForceStop -- TODO: don't think we should need this
  deriving (Show, Generic)

-- TODO: investigate
thereIsBiggerMove :: CantStopPlayName -> CantStopPlayName -> Bool
thereIsBiggerMove (OneMove u) (TwoMove s t) = (s == u) || (t == u)
thereIsBiggerMove _ _ = False

instance Eq CantStopPlayName where
  (TwoMove s t) == (TwoMove s' t') = ((s == s' && t == t') || (s == t' && t == s'))
  (OneMove s) == (OneMove s') = s == s'
  Stop == Stop = True
  DontStop  == DontStop   = True
  ForceStop  == ForceStop  = True
  _ == _ = False

deriving instance Ord CantStopPlayName

data CantStopPhaseName = CSTurn Player deriving (Eq, Ord, Show, Generic)

playerTurn :: Player -> CantStopTurn
playerTurn p = Turn p (NE.singleton (CSTurn p))

type CantStopTurn = Turn CantStopPhaseName

type CantStopPhase = Phase CantStopPhaseName CantStopLocation CantStopCounterName CantStopResource CantStopPlayName

type CantStopAction = GameAction CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName

type CantStopGameState = GameState CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName

type CantStopOptions = Options CantStopPlayName

type CantStopGameRules = GameRules CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName

type CSM a = GameRule CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName a

type CSView = GameStateView CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName

