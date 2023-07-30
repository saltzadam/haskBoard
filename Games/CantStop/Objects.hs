{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Objects where

import Data.Finitary (Finitary)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import FinitaryMap (FTMap (..))
import GHC.Generics (Generic)
import Game.GameNode (GameAction, GameNode)
import Game.GameState
import Game.Location
import Game.Monad
import Game.Options (Legality (..), Options, oneIssue)
import Game.Player
import Game.Rules
import Game.View (GameStateView)
import Track (Track (..))

data TrackName = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Eleven | Twelve
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Finitary)

data TrackHeight = HOne | HTwo | HThree | HFour | HFive | HSix | HSeven | HEight | HNine | HTen | HEleven | HTwelve | HThirteen deriving (Eq, Ord, Show, Enum, Generic, Finitary)

data CantStopLocation
  = BoxTop
  | TrackSpot TrackName TrackHeight
  deriving (Eq, Ord, Show, Generic, Finitary)

maxSlot :: TrackName -> Int
maxSlot t =
  if t <= Seven
    then trackNum t
    else 24 - trackNum t
  where
    trackNum t = 2 * fromEnum t + 2

track :: TrackName -> Track CantStopLocation
track name = Track (NE.fromList [TrackSpot name slot | slot <- [HOne .. toEnum (maxSlot name)]])

diceToTrack :: Int -> TrackName
diceToTrack x = toEnum . fromEnum $ (x - 2)

data CantStopResource = PlayerMarker Player | TemporaryMarker deriving (Eq, Ord, Show, Generic, Finitary)

data CantStopCounterName = DieOne | DieTwo | DieThree | DieFour
  deriving (Eq, Ord, Show, Generic, Finitary, Enum)

type CantStopLocations = Locations CantStopLocation CantStopResource

type CantStopCounters = Counters CantStopCounterName

type CantStopGameObjects = GameObjects CantStopLocation CantStopCounterName CantStopResource

theDice :: [CantStopCounterName]
theDice = [DieOne .. DieFour]

marker :: Maybe Player -> CantStopResource
marker (Just p) = PlayerMarker p
marker Nothing = TemporaryMarker

owner :: CantStopResource -> Maybe Player
owner (PlayerMarker p) = Just p
owner TemporaryMarker = Nothing

initLocations' :: Set Player -> CantStopLocation -> LocationShape CantStopResource
-- initLocations' _ (TrackSpot _ _) = Deck Seq.empty
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

data CantStopIssue
  = NotEnoughMarkers
  | TrackCompleted
  | AtTop
  | CanMoveTwo
  deriving (Eq, Ord, Show, Generic)

data CantStopPlayName
  = TwoMove Player TrackName TrackName
  | OneMove Player TrackName
  | Stop Player
  | DontStop Player
  | ForceStop Player
  deriving (Show, Generic)

thereIsBiggerMove :: CantStopPlayName -> CantStopPlayName -> Legality CantStopIssue
thereIsBiggerMove (OneMove _ u) (TwoMove _ s t) = if (s == u) || (t == u) then oneIssue CanMoveTwo else Legal
thereIsBiggerMove _ _ = Legal

instance Eq CantStopPlayName where
  (TwoMove p s t) == (TwoMove p' s' t') = p == p' && ((s == s' && t == t') || (s == t' && t == s'))
  (OneMove p s) == (OneMove p' s') = p == p' && s == s'
  (Stop p) == (Stop p') = p == p'
  (DontStop p) == (DontStop p') = p == p'
  (ForceStop p) == (ForceStop p') = p == p'
  _ == _ = False

deriving instance Ord CantStopPlayName

data CantStopPhaseName = CSTurn Player deriving (Eq, Ord, Show, Generic)

type CantStopTurn = Turn CantStopPhaseName

type CantStopPhase = Phase CantStopPhaseName CantStopLocation CantStopCounterName CantStopResource CantStopPlayName CantStopIssue

type CantStopAction = GameAction CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName

type CantStopGameState = GameState CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName CantStopIssue

type CantStopOptions = Options CantStopPlayName CantStopIssue

type CantStopGameRules = GameRules CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName CantStopIssue

type CantStopGameNode = GameNode CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName CantStopIssue

-- type CSM a = GameEff CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName CantStopIssue a
type CSM a = GameRule CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName CantStopIssue a

type CSView = GameStateView CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName

playerTurn :: Player -> CantStopTurn
playerTurn p = Turn p (NE.singleton (CSTurn p))
