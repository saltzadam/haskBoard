{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Objects 
    where
import GHC.Generics (Generic)
import Data.Finitary (Finitary, inhabitants)
import Game.Player
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Location
import Data.Set (Set)
import qualified Data.Sequence as Seq
import qualified Data.Map as M
import qualified Data.Set as S
import GameE (Phase, GameState, Game, ObserveGame, Turn)
import GameNode (GameAction, GameNode)
import Count (Cnt)
import FinitaryMap (FTMap(..))
-- Does it make more sense to have an enum or just a newtype int?

data TrackName = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Eleven | Twelve
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Finitary)

data TrackHeight = HOne | HTwo | HThree | HFour | HFive | HSix | HSeven | HEight | HNine | HTen | HEleven | HTwelve | HThirteen deriving (Eq, Ord, Show, Enum, Generic)

instance Finitary TrackHeight

diceToTrack :: Cnt Int -> TrackName
diceToTrack x = toEnum . fromEnum $ (x - 2)


maxSlot :: TrackName -> TrackHeight
maxSlot t = if t <= Seven
            then toEnum (trackNum t)
            else toEnum (24 - trackNum t )
                where
                    trackNum :: TrackName -> Int
                    trackNum t = 2*(fromEnum t) +2

getHeight :: TrackHeight -> Int
getHeight h = fromEnum h + 1

data CantStopResource = PlayerMarker Player | TemporaryMarker deriving (Eq, Ord, Show, Generic)

markerOwner :: CantStopResource -> Maybe Player
markerOwner (PlayerMarker p) = Just p
markerOwner _ = Nothing

data CantStopLocation
  = TrackSpot TrackName TrackHeight
  | BoxTop
  | PlayerStuff Player
  deriving (Eq, Ord, Show, Generic)

maxSpot :: TrackName -> CantStopLocation
maxSpot s = TrackSpot s (maxSlot s)

-- TODO: what value are we getting from NonEmpty
trackSlots :: TrackName -> NonEmpty CantStopLocation
trackSlots track = NE.fromList $ TrackSpot track <$> [HOne .. maxSlot track]

data CantStopCounterName = DieOne | DieTwo | DieThree | DieFour
  deriving (Eq, Ord, Show, Generic, Enum)
  deriving anyclass (Finitary)
type CantStopLocations = Locations CantStopLocation CantStopResource
type CantStopCounters = Counters CantStopCounterName
type CantStopGameObjects = GameObjects CantStopLocation CantStopCounterName CantStopResource

allSpots :: [CantStopLocation]
allSpots = [TrackSpot name height | name <- inhabitants, height <- [HOne .. maxSlot name]]

theDiceL :: (CantStopCounterName, CantStopCounterName, CantStopCounterName, CantStopCounterName)
theDiceL = (DieOne, DieTwo, DieThree, DieFour)

initLocations' :: Set Player -> CantStopLocation -> LocationShape CantStopResource
initLocations' _ (TrackSpot _ _) = Deck Seq.empty
initLocations' _ BoxTop = Pile (M.singleton TemporaryMarker 3)
initLocations' players (PlayerStuff player)
    | player `S.member` players = Pile (M.singleton (PlayerMarker player) 11)
    | otherwise = Dummy

initLocations :: Set Player -> CantStopLocations -- FTMap CantStopLocation (LocationShape CantStopResource)
initLocations ps = FTMap (initLocations' ps)

initDice' :: CantStopCounterName -> Counter
initDice' = const d6

initGameObjects :: Set Player -> CantStopGameObjects
initGameObjects ps =
  GameObjects
    { locations = initLocations ps,
      counters = FTMap initDice'
    }


data Issue = ThreeTempMarkersOut | TrackCompleted | AtTop deriving (Eq, Ord, Show, Generic)
data PlayName = Move Player TrackName TrackName | Stop Player | DontStop Player deriving (Eq, Ord, Show, Generic)
data CantStopPhaseName = CSTurn Player deriving (Eq, Ord, Show, Generic)
type CantStopTurn = Turn CantStopPhaseName
type CantStopPhase = Phase CantStopPhaseName CantStopLocation CantStopCounterName CantStopResource PlayName Issue
type CantStopAction = GameAction CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName

-- type CantStopGame = Game CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName PlayName CantStopTurns Player

type CantStopGameState = GameState CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName PlayName Issue
type CantStopGame = Game  CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName PlayName Issue
type CantStopGameNode = GameNode CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName PlayName Issue
-- type CantStopGetOptions = GetOptions CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName PlayName Issue
type Observe es = ObserveGame CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName PlayName Issue es

currentPlayer :: CantStopPhaseName ->Player
currentPlayer (CSTurn p) = p


