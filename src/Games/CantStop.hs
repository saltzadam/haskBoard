{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDeriving #-}
module Games.CantStop where

import Util
import Game.Player (Player)
import GHC.Generics
import Game
import qualified Data.Map as M
import Location (Locations, LocationShape (..), Counter(..), GameObjects (..), counters, makeCounter, rollCounter)
import Data.Map (Map)
import Count
import Data.Tuple (swap)
import Game.Condition
import Data.Bitraversable
import Control.Monad.Trans.Reader
import Control.Monad.Random (mkStdGen)
import Game.Control (nextCyclic)
import Control.Monad.State.Lazy
import Control.Lens (modifying, at, ix, (?=), (<~), (%=), (%~))


data TrackName = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Eleven | Twelve
    deriving (Eq, Ord, Show, Enum, Bounded)


trackNum :: TrackName -> Int
trackNum t = fromEnum t + 2

maxSlot :: TrackName -> Int
maxSlot t =  trackNum t + 1

type TrackHeight = Int

-- Resource -> Condition
-- Resource -> Game
-- Resource -> Phase
-- Resource -> Location
-- Location -> Condition
-- Location -> Game
-- Location -> Phase
-- PhaseName -> Condition
-- PhaseName -> Game
-- PhaseName -> Phase
-- PhaseName -> Play
-- Play -> Game
-- Play -> Conition

-- Resource
-- Location
-- PlayName
-- PhaseName
-- Play
-- Game Condition


data CantStopResource = PlayerMarker Player | TemporaryMarker deriving (Eq, Ord, Show, Generic)
data CantStopLocation = TrackSpot TrackName TrackHeight
                | BoxTop
                | PlayerStuff Player
                | DieOne | DieTwo | DieThree | DieFour
                deriving (Eq, Ord, Show, Generic)

type CantStopLocations = Locations CantStopLocation CantStopResource
type CantStopGameObjects = GameObjects CantStopLocation CantStopResource

theDice :: (CantStopLocation, CantStopLocation, CantStopLocation,
 CantStopLocation)
theDice = (DieOne, DieTwo, DieThree, DieFour)

theDiceL :: [CantStopLocation]
theDiceL = [DieOne, DieTwo, DieThree, DieFour]

-- what's the generic way to do this
initTrackSlots :: CantStopLocations
initTrackSlots = M.fromList [(TrackSpot name height, Slot Nothing) | name <- enumerateFromRoot, height <- [1..maxSlot name]]

initBoxTop :: CantStopLocations
initBoxTop = M.singleton BoxTop (Pile (M.singleton TemporaryMarker 3))

initPlayerL :: [Player] -> CantStopLocations
initPlayerL ps = M.fromList [(PlayerStuff player, Pile (M.singleton (PlayerMarker player) 3)) | player <- ps]

initDice :: Map CantStopLocation Counter
initDice = M.fromList (zip theDiceL (repeat (makeCounter (1,6))))

initGameObjects :: [Player] -> CantStopGameObjects
initGameObjects ps = GameObjects {
    locations = initPlayerL ps <> initTrackSlots <> initBoxTop,
    counters = initDice}

rollDie :: Counter -> Counter
rollDie = rollCounter

rollDice' :: [CantStopAction]
rollDice' = RollCounter <$> theDiceL 


rollDice :: CantStopCondition [CantStopAction]
rollDice = return rollDice'

data CantStopTriggers deriving (Eq, Ord, Show, Generic)
data MoveArity = TwoValueMove TrackName TrackName | OneValueMove TrackName deriving (Eq, Ord, Show, Generic)

data CantStopPlayName = Move Player MoveArity | Stop Player deriving (Eq, Ord, Show, Generic)



data CantStopPhaseName = Turn Player deriving (Eq, Ord, Show, Generic)
type CantStopTurns = Int
type CantStopPhase = Phase CantStopPhaseName CantStopLocation CantStopResource CantStopPlayName CantStopTurns CantStopTriggers

type CantStopAction = GameAction CantStopLocation CantStopResource CantStopPhaseName

type CantStopCondition val = Condition CantStopLocation CantStopResource CantStopPhaseName CantStopPlayName CantStopTurns CantStopTriggers val
type CantStopGame = Game CantStopLocation CantStopResource CantStopPhaseName CantStopPlayName CantStopTurns CantStopTriggers





playerTurn :: Player -> CantStopPhase
playerTurn p = Phase {
    name = Turn p,
    enterAction = rollDice <> undefined, -- first roll, then choice
    exitAction = undefined, -- check winner
    legal = undefined,
    control = undefined
                     }



-- add condition
moveLegal :: CantStopPlayName -> CantStopCondition Bool
moveLegal (Stop _) = cTrue
moveLegal (Move _ (TwoValueMove s t)) = cIn <*> pure (Cnt $ trackNum s, Cnt $ trackNum t) <*> diceVals
moveLegal (Move _ (OneValueMove s)) = cIn <*> pure (Cnt $ trackNum s, Cnt $ trackNum s) <*> diceVals

diceVals :: CantStopCondition [(Cnt Int, Cnt Int)]
diceVals = mapM (bitraverse makeSum makeSum) (mkPairs theDice) where
    makeSum (c, c') = cCounterVal c + cCounterVal c'
    mkPairs :: (a,a,a,a) -> [((a,a),(a,a))]
    mkPairs (x,y,w,z) = let
        pairs1 = [((x,y),(w,z)),((x,w),(y,z)),((x,z),(y,w))]
        pairs2 = fmap swap pairs1
                         in pairs1 ++ pairs2


cantStopPlays :: CantStopPlayName -> CantStopCondition [CantStopAction]
cantStopPlays (Stop p) = undefined
cantStopPlays (Move p (TwoValueMove s t)) = sequence [moveup p s, moveup p t]
cantStopPlays (Move p (OneValueMove s)) = sequence [moveup p s, moveup p s]

initGame :: [Player] -> CantStopGame
initGame ps = Game {
    players = ps,
    objects = initGameObjects ps,
    runPlay = undefined,
    randGen = mkStdGen 100,
    triggers = [],
    chooser = undefined,
    advancePlayer = nextCyclic,
    activePlayer=Nothing,
    turnNumber=1
                   }
-- mkList :: (Eq a, Show a) => [C2 l r ph a] -> C2 l r ph [a]
-- mkList = foldr Cons Empty

moveup :: Player -> TrackName -> CantStopCondition CantStopAction
moveup p track = Condition $ do
    g <- get
    return $ constructTransfer (currTempMarkerSpot g) (currPlayerMarkerSpot g)
        where
            trackSpots = [TrackSpot track num | num <- enumerateFromRoot, num <= maxSlot track]
            currTempMarkerSpot g = dropWhile (not . cHas g TemporaryMarker) trackSpots
            currPlayerMarkerSpot g = dropWhile (not . cHas g (PlayerMarker p)) trackSpots
            constructTransfer :: [CantStopLocation] -> [CantStopLocation] -> CantStopAction
            constructTransfer (curr:next:_) _ = MkTransfer curr next TemporaryMarker 
            constructTransfer [_] _ = DoNothing
            constructTransfer [] (curr:_:_) = MkTransfer BoxTop curr TemporaryMarker 
            constructTransfer _ [_] = DoNothing
            constructTransfer [] [] = MkTransfer BoxTop (TrackSpot track 1) TemporaryMarker 




