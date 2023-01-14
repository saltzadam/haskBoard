{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveAnyClass #-}
module CantStop where

import Util
import Game.Player (Player)
import GHC.Generics
import Game
import Location (Locations, LocationShape (..), Counter(..), GameObjects (..), counters, Counters, d6)
import Count
import Data.Tuple (swap)
import Game.Condition
import Data.Bitraversable
import Control.Monad.Random (mkStdGen)
import Control.Monad.State.Lazy
import Data.Finitary (Finitary, inhabitants)
import Defaultable.Map as D
import qualified Data.Sequence as Seq
import FinitaryMap
import GHC.Base (liftA2)
import Control.Lens (uses, use)
import Data.Maybe (fromJust, listToMaybe)


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
type CantStopGame = Game CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName  CantStopTurns CantStopTriggers
type CantStopGameNode = GameNode CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName   CantStopTurns CantStopTriggers
type CantStopChoice = Choice  CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName  CantStopTurns CantStopTriggers

type CantStopGameS = State CantStopGame
type CantStopActionS = CantStopGameS CantStopAction

rollDice' :: [CantStopAction]
rollDice' = RollCounter <$> (inhabitants :: [CantStopCounterName])

rollDice :: CantStopCondition [CantStopAction]
rollDice = return rollDice'

playerTurn :: Player -> CantStopPhase
playerTurn p = Phase {
    name = Turn p,
    seedNodes = [rollNode, chooseToRollOrStopNode p]
                     }


-- good spot for a helper, esp w/ `parents`
rollNode :: CantStopGameNode
rollNode = GameNode {
        node = Right rollDice',
        source = Nothing,
        parents = [],
        owner = Nothing
                    }

chooseToRollOrStopNode :: Player -> CantStopGameNode -- (CantStopGameS [CantStopPlayName])
chooseToRollOrStopNode p = GameNode {
    node = Left (chooseToRollOrStop p),
    source = Nothing,
    owner = Just p,
    parents = []
                                  }

chooseToRollOrStop :: Player -> CantStopChoice
chooseToRollOrStop p = liftA2 (++) (legalRolls p) (pure [Stop p]) where
    legalRolls :: Player -> CantStopGameS [CantStopPlayName]
    legalRolls p = fmap (fmap (makeRoll p)) diceVals
    makeRoll :: Player -> (Cnt Int, Cnt Int) -> CantStopPlayName
    makeRoll p (x,y) = if x == y 
                        then Move p (OneValueMove (coerceEnum x))
                        else Move p (TwoValueMove (coerceEnum x) (coerceEnum y))




diceVals :: CantStopGameS [(Cnt Int, Cnt Int)]
diceVals = runCondition $ mapM (bitraverse makeSum makeSum) (mkPairs theDiceL) where
    makeSum (c, c') = cCounterVal c + cCounterVal c'
    mkPairs :: (a,a,a,a) -> [((a,a),(a,a))]
    mkPairs (x,y,w,z) = let
        pairs1 = [((x,y),(w,z)),((x,w),(y,z)),((x,z),(y,w))]
        pairs2 = fmap swap pairs1
                         in pairs1 ++ pairs2


advancePlayer :: CantStopGameS Player
advancePlayer = do
    ps <- use #players
    p  <- use #activePlayer
    case p of
      Nothing -> return $ head ps
      Just p' -> return (fromJust $ nextCyclic p' ps) -- Lame

cantStopPlays :: CantStopPlayName -> [CantStopActionS]
cantStopPlays (Stop _) = [ChangePhase . Turn <$> advancePlayer]
cantStopPlays (Move p (TwoValueMove s t)) = [moveup p s, moveup p t]
cantStopPlays (Move p (OneValueMove s)) = [moveup p s, moveup p s]

initGame :: [Player] -> CantStopGame
initGame ps = Game {
    players = ps,
    objects = initGameObjects,
    runPlay = undefined,
    randGen = mkStdGen 100,
    chooser = undefined,
    activePlayer=Nothing,
    turnNumber=1
                   }

moveup :: Player -> TrackName -> CantStopActionS
moveup p track = do
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




