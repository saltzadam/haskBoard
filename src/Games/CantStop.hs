{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}
module Games.CantStop where

import Util
import Game.Player (Player)
import Game.Game (Game, roll)
import GHC.Generics
import Game (Phase(..))
import qualified Data.Map as M
import Location (Locations, LocationShape (..), Counter(..))
import Data.Map (Map)


data TrackName = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Eleven | Twelve
    deriving (Eq, Ord, Show, Enum, Bounded)


toNum :: TrackName -> Int
toNum t = fromEnum t + 2

maxSlot :: TrackName -> Int
maxSlot t = toNum t + 1

type TrackHeight = Int

data Location = TrackSpot TrackName TrackHeight
                | BoxTop
                | PlayerStuff Player
                | DieOne | DieTwo | DieThree | DieFour
                deriving (Eq, Ord, Show, Generic)
data Resource = PlayerMarker Player | TemporaryMarker

theDice :: [Location]
theDice = [DieOne, DieTwo, DieThree, DieFour]

-- what's the generic way to do this
initTrackSlots :: Locations Location Resource
initTrackSlots = M.fromList [(TrackSpot name height, Slot Nothing) | name <- enumerateFromRoot, height <- [1..maxSlot name]]

initBoxTop :: Locations Location Resource
initBoxTop = M.singleton BoxTop (Pile (M.singleton TemporaryMarker 3))

initPlayerL :: [Player] -> Locations Location Resource
initPlayerL ps = M.fromList [(PlayerStuff player, Pile (M.singleton (PlayerMarker player) 3)) | player <- ps]

initDice :: Map Location Counter
initDice = M.fromList [(die, Counter Nothing (1,6)) | die <- theDice]

data PhaseName = Roll | PlayerTurn Player deriving (Eq, Ord, Show, Generic)

doNothing :: p1 -> p2 -> [a]
doNothing _ _ = []

type CantStopGame = Game Location Resource PhaseName
type CantStopPhase = Phase PhaseName Location Resource

rollAction :: Game Location r p2 -> Game Location r p2
rollAction = compose [fst . roll die | die <- theDice]





