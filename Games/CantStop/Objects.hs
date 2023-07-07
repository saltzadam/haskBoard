{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Objects
    where
import GHC.Generics (Generic)
import Game.Player
import Game.Location
import Data.Set (Set)
import qualified Data.Sequence as Seq
import qualified Data.Map as M
import qualified Data.Set as S
import Game.GameState
import Game.GameNode (GameAction, GameNode)
import Game.Monad
import Game.View (GameStateView)
import Game.Options (Options, Legality (..), oneIssue)
import Data.Finitary (Finitary, inhabitants)
import qualified Data.List.NonEmpty as NE
import FinitaryMap (FTMap(..))
import Data.Maybe (isJust, fromMaybe)
import Control.Lens
import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty)

data TrackName = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Eleven | Twelve
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Finitary)

-- data TrackHeight = HOne | HTwo | HThree | HFour | HFive | HSix | HSeven | HEight | HNine | HTen | HEleven | HTwelve | HThirteen deriving (Eq, Ord, Show, Enum, Generic, Finitary)

track :: Int -> Counter
track i = Counter Nothing (1,i)

diceToTrack ::  Int -> TrackName
diceToTrack x = toEnum . fromEnum $ (x - 2)


maxSlot :: TrackName -> Int
maxSlot t = if t <= Seven
            then trackNum t
            else 24 - trackNum t 
                where
                    trackNum t = 2*(fromEnum t) +2

-- getHeight :: TrackHeight -> Int
-- getHeight h = fromEnum h + 1

data CantStopResource = PlayerMarker Player | TemporaryMarker deriving (Eq, Ord, Show, Generic, Finitary)

markerOwner :: CantStopResource -> Maybe Player
markerOwner (PlayerMarker p) = Just p
markerOwner _ = Nothing

data CantStopLocation
  = BoxTop
  | Board
  deriving (Eq, Ord, Show, Generic, Finitary)

-- makePrisms ''CantStopLocation

-- parseSpotLens :: CantStopLocation -> Maybe TrackName
-- parseSpotLens loc = loc ^? _TrackSpot . to fst

-- isSpot :: CantStopLocation -> Bool
-- isSpot = isJust . parseTrackSpot

-- parseTrackSpot :: CantStopLocation -> Maybe (TrackName, TrackHeight)
-- parseTrackSpot (TrackSpot n h) = Just (n,h)
-- parseTrackSpot _ = Nothing

-- sameTrack :: CantStopLocation -> CantStopLocation -> Bool
-- sameTrack (TrackSpot n _) (TrackSpot n' _) = n == n'
-- sameTrack _ _ = False

-- maxSpot :: TrackName -> CantStopLocation
-- maxSpot s = TrackSpot s (maxSlot s)

-- trackSlots :: TrackName -> [CantStopLocation]
-- trackSlots track = TrackSpot track <$> [HOne .. maxSlot track]

data DieNum = DieOne | DieTwo | DieThree | DieFour deriving (Eq, Ord, Show, Generic, Enum, Bounded, Finitary)

getTrack :: CantStopCounterName -> Maybe TrackName
getTrack (PlayerTrack _ t) = Just t
getTrack (TempTrack t) = Just t
getTrack (Die _) = Nothing

getTrackOwner :: CantStopCounterName -> Maybe Player
getTrackOwner (PlayerTrack p _) = Just p
getTrackOwner _ = Nothing

data CantStopCounterName = Die DieNum | PlayerTrack Player TrackName | TempTrack TrackName
  deriving (Eq, Ord, Show, Generic, Finitary)
type CantStopLocations = Locations CantStopLocation CantStopResource
type CantStopCounters = Counters CantStopCounterName
type CantStopGameObjects = GameObjects CantStopLocation CantStopCounterName CantStopResource

-- allSpots :: [CantStopLocation]
-- allSpots = [TrackSpot name height | name <- inhabitants, height <- [HOne .. maxSlot name]]

theDiceL :: (CantStopCounterName, CantStopCounterName, CantStopCounterName, CantStopCounterName)
theDiceL = (Die DieOne, Die DieTwo, Die DieThree, Die DieFour)

initLocations' :: Set Player -> CantStopLocation -> LocationShape CantStopResource
-- initLocations' _ (TrackSpot _ _) = Deck Seq.empty
initLocations' _ BoxTop = Pile (M.singleton TemporaryMarker 3)
initLocations' _ Board = Pile M.empty
-- initLocations' players (PlayerStuff player)
--     | player `S.member` players = Pile (M.singleton (PlayerMarker player) 11)
--     | otherwise = Dummy

initLocations :: Set Player -> CantStopLocations
initLocations = FTMap . initLocations'

-- initDice' :: CantStopCounterName -> Counter
-- initDice' = const d6

initCounters :: CantStopCounterName -> Counter
initCounters (Die _) = d6
initCounters (PlayerTrack _ trackName) = makeCounter (1,maxSlot trackName)
initCounters (TempTrack trackName) = makeCounter (1, maxSlot trackName)

initGameObjects :: Set Player -> CantStopGameObjects
initGameObjects ps =
  GameObjects
    { locations = initLocations ps,
      counters = FTMap initCounters
    }


data CantStopIssue = NotEnoughMarkers
                   | TrackCompleted
                   | AtTop
                   | CanMoveTwo
                   deriving (Eq, Ord, Show, Generic)

data CantStopPlayName = TwoMove Player TrackName TrackName
                      | OneMove Player TrackName
                      | Stop Player
                      | DontStop Player
                      | ForceStop Player deriving (Show, Generic)


makePrisms ''CantStopPlayName

-- TODO: this is PartialOrd
-- isWorseMove :: CantStopPlayName -> CantStopPlayName -> Bool



decomposeMap :: Player -> Map CantStopPlayName (NonEmpty CantStopPlayName)
decomposeMap p = M.fromList [(TwoMove p s t, NE.fromList [OneMove p s, OneMove p t]) | s <- [Two .. Twelve], t <- [Two .. Twelve]] 

isTwoMove :: CantStopPlayName -> Bool
isTwoMove (TwoMove {}) = True
isTwoMove _ = False

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
type CSM a = GameEff CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName CantStopIssue a

type CSView = GameStateView CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName

playerTurn :: Player -> CantStopTurn
playerTurn p = Turn p (NE.singleton (CSTurn p))

allPlayers :: [Player]
allPlayers = mkPlayers 4

allTracks :: [CantStopCounterName]
allTracks = PlayerTrack <$> allPlayers <*> [Two .. Twelve]
