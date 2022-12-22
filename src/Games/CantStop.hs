{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
    {-# LANGUAGE ScopedTypeVariables #-}
        {-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
module Games.CantStop where

import Util
import Game.Player (Player)
import GHC.Generics
import Game
import qualified Data.Map as M
import Location (Locations, LocationShape (..), Counter(..), GameObjects, counters, makeCounter, rollCounter)
import Data.Map (Map)
import Control.Lens (bimap)
import Count


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


-- -- if there's an Eq constraint here then there may as well be below...
-- sumsOfPairs :: (Eq a, Num a) => [a] -> [[a]]
-- sumsOfPairs = nub . sumsOfPairs'

-- -- all the ways to sum a roll
-- -- works for any even number of dice, i.e. overengineered
-- sumsOfPairs' :: forall a. Num a => [a] -> [[a]]
-- sumsOfPairs' as = sumPairs (mkPairs as) where
--     -- pigworker here https://stackoverflow.com/questions/12869097/splitting-list-into-a-list-of-possible-tuples
--     sumPairs :: Num a => [[(a,a)]] -> [[a]]
--     sumPairs = fmap (fmap (uncurry (+)))
-- mkPairs :: [a] -> [[(a,a)]]
-- mkPairs [] = [[]]
-- mkPairs (a:as) = [(a,b):bs | (preb,b,postb) <- zippers as, bs <- mkPairs (preb++postb) ] where
--     zippers :: [a] -> [([a],a,[a])]
--     zippers as = go as [] where
--         go :: [a] -> [([a],a,[a])] -> [([a],a,[a])]
--         -- assume first list is in 'reverse' order
--         -- [1,2,3,4] -> [([],1,[2,3,4]), ([1],2,[3,4]), ([2,1],3,[4]), ([3,2,1],4,[])
--         go (x:xs) [] = go xs [([],x,xs)]
--         go (x:xs) (y@(y0,y1,_):ys) = go xs ((y1:y0, x, xs) : (y:ys))
--         go [] ys = ys

mkPairs :: (a,a,a,a) -> [((a,a),(a,a))]
mkPairs (x,y,w,z) = [((x,y),(w,z)),((x,w),(y,z)),((x,z),(y,w))]

data CantStopPlayName = Move (Either (TrackName, TrackName) TrackName) | Stop
data CantStopPhaseName = Roll | PlayerChoice deriving (Eq, Ord, Show, Generic)

type CantStopPhase = Phase CantStopPhaseName CantStopLocation CantStopResource CantStopPlayName

type CantStopCondition val = C2 CantStopLocation CantStopResource CantStopPhaseName val
type CantStopGame = Game CantStopLocation CantStopResource CantStopPhaseName

rollDice :: Counter -> Counter
rollDice = rollCounter

allPlays :: [CantStopPlayName]
allPlays = Stop : [Move (Left (s, t)) | s <- enumerateFromRoot, t <- enumerateFromRoot, s <= t]
                ++ [Move (Right s) | s <- enumerateFromRoot]

chooseTracksPhase :: Player -> CantStopPhase
chooseTracksPhase p = Phase {name = PlayerChoice,
                             enterAction = Empty,
                             exitAction = Empty,
                             control = One p,
                             possiblePlays = allPlays,
                             legal = undefined
                            }


unorderedIn :: (Show a, Eq a) => CantStopCondition (a,a) -> CantStopCondition [(a,a)] -> CantStopCondition Bool
unorderedIn c l = (c `In` l) `Or` (cSwap c `In` l)
    where
        cSwap :: (Show a, Show b) => CantStopCondition (a,b) -> CantStopCondition (b,a)
        cSwap c = Pair `Apply` Apply Snd c `Apply` Apply Fst c

moveLegal :: CantStopPlayName -> CantStopCondition Bool
moveLegal Stop = true
moveLegal (Move (Left (s,t))) = (Pair `Apply` Lit (Cnt $ trackNum s) `Apply` Lit (Cnt $ trackNum t)) `unorderedIn` diceVals
moveLegal (Move (Right s)) = undefined

diceVals :: CantStopCondition [(Cnt Int, Cnt Int)]
diceVals =  mkList $ fmap (\(a,b) -> Pair `Apply` a `Apply` b) (bimap (uncurry makeSum) (uncurry makeSum) <$> mkPairs theDice) where
    makeSum c c' = CounterVal2 c + CounterVal2 c'


mkList :: (Eq a, Show a) => [C2 l r ph a] -> C2 l r ph [a]
mkList = foldr Cons Empty

-- moveup :: TrackName -> TrackName -> Play
-- moveup track = Play {name = (MoveUp track),
--                      legalCondition = 




