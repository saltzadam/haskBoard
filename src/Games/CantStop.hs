{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
module Games.CantStop where

import Util
import Game.Player (Player)
import GHC.Generics
import Game
import qualified Data.Map as M
import Location (Locations, LocationShape (..), Counter(..), GameObjects (..), counters, makeCounter, rollCounter, Counters, d6)
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
import Data.Finitary (Finitary, inhabitants)
import Defaultable.Map as D
import qualified Data.Sequence as Seq
import FinitaryMap


data TrackName = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Eleven | Twelve
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)
    deriving anyclass (Finitary)


trackNum :: TrackName -> Int
trackNum t = fromEnum t + 2

maxSlot :: TrackName -> Int
maxSlot t =  trackNum t + 1

type TrackHeight = Int


data CantStopResource = PlayerMarker Player | TemporaryMarker deriving (Eq, Ord, Show, Generic)
data CantStopLocation = TrackSpot TrackName TrackHeight
                | BoxTop
                | PlayerStuff Player
                deriving (Eq, Ord, Show, Generic)

data CantStopCounterName = DieOne | DieTwo | DieThree | DieFour
    deriving (Eq, Ord, Show, Generic, Enum)
    deriving anyclass (Finitary)

type CantStopLocations = Locations CantStopLocation CantStopResource
type CantStopCounters = Counters CantStopCounterName
type CantStopGameObjects = GameObjects CantStopLocation CantStopCounterName CantStopResource

theDiceL :: (CantStopCounterName, CantStopCounterName, CantStopCounterName, CantStopCounterName)
theDiceL = (DieOne, DieTwo, DieThree, DieFour)
--
-- TODO: should these be singletons with default 0? Could make a helper for that.
initLocations' :: CantStopLocation -> LocationShape CantStopResource 
initLocations' (TrackSpot _ _) = Deck Seq.empty
initLocations' BoxTop = Pile (D.singleton (TemporaryMarker,3))
initLocations' (PlayerStuff player) = Pile (D.singleton (PlayerMarker player, 11))

initLocations :: CantStopLocations --FTMap CantStopLocation (LocationShape CantStopResource)
initLocations = FTMap initLocations'

initDice' :: CantStopCounterName -> Counter
initDice' = const d6

initDice :: CantStopCounters
initDice = FTMap initDice'

initGameObjects ::  CantStopGameObjects
initGameObjects = GameObjects {
    locations = initLocations,
    counters = initDice}

data CantStopTriggers deriving (Eq, Ord, Show, Generic)
data MoveArity = TwoValueMove TrackName TrackName | OneValueMove TrackName deriving (Eq, Ord, Show, Generic)

data CantStopPlayName = Move Player MoveArity | Stop Player deriving (Eq, Ord, Show, Generic)


data CantStopPhaseName = Turn Player deriving (Eq, Ord, Show, Generic)
type CantStopTurns = Int
type CantStopPhase = Phase CantStopPhaseName CantStopLocation CantStopCounterName CantStopResource CantStopPlayName CantStopTurns CantStopTriggers

type CantStopAction = GameAction CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName

type CantStopCondition val = Condition CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName CantStopTurns CantStopTriggers val
type CantStopGame = Game CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName CantStopTurns CantStopTriggers

rollDice' :: [CantStopAction]
rollDice' = RollCounter <$> (inhabitants :: [CantStopCounterName])

rollDice :: CantStopCondition [CantStopAction]
rollDice = return rollDice'

playerTurn :: Player -> CantStopPhase
playerTurn p = Phase {
    name = Turn p,
    seedNodes = undefined
                     }

-- add condition
moveLegal :: CantStopPlayName -> CantStopCondition Bool
moveLegal (Stop _) = cTrue
moveLegal (Move _ (TwoValueMove s t)) = cIn <*> pure (Cnt $ trackNum s, Cnt $ trackNum t) <*> diceVals
moveLegal (Move _ (OneValueMove s)) = cIn <*> pure (Cnt $ trackNum s, Cnt $ trackNum s) <*> diceVals

diceVals :: CantStopCondition [(Cnt Int, Cnt Int)]
diceVals = mapM (bitraverse makeSum makeSum) (mkPairs theDiceL) where
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
    objects = initGameObjects,
    runPlay = undefined,
    randGen = mkStdGen 100,
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




