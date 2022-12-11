{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
    {-# LANGUAGE ScopedTypeVariables #-}
module Games.CantStop where

import Util
import Game.Player (Player)
import Game.Game (Game, roll)
import GHC.Generics
import Game (Phase(..), Play)
import qualified Data.Map as M
import Location (Locations, LocationShape (..), Counter(..), GameObjects, counters)
import Data.Map (Map)
import Game.Condition
import Control.Lens (view, ix, at, preview, bimap)
import Count
import Control.Monad (guard, join)
import Data.Tuple (swap)
import Data.Bitraversable (bisequence, bimapM)


data TrackName = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Eleven | Twelve
    deriving (Eq, Ord, Show, Enum, Bounded)


toNum :: TrackName -> Int
toNum t = fromEnum t + 2

maxSlot :: TrackName -> Int
maxSlot t = toNum t + 1

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


data CantStopResource = PlayerMarker Player | TemporaryMarker
data CantStopLocation = TrackSpot TrackName TrackHeight
                | BoxTop
                | PlayerStuff Player
                | DieOne | DieTwo | DieThree | DieFour
                deriving (Eq, Ord, Show, Generic)

type CantStopLocations = Locations CantStopLocation CantStopResource
type CantStopGameObjects = GameObjects CantStopLocation CantStopResource

theDice :: [CantStopLocation]
theDice = [DieOne, DieTwo, DieThree, DieFour]

-- what's the generic way to do this
initTrackSlots :: CantStopLocations
initTrackSlots = M.fromList [(TrackSpot name height, Slot Nothing) | name <- enumerateFromRoot, height <- [1..maxSlot name]]

initBoxTop :: CantStopLocations
initBoxTop = M.singleton BoxTop (Pile (M.singleton TemporaryMarker 3))

initPlayerL :: [Player] -> CantStopLocations
initPlayerL ps = M.fromList [(PlayerStuff player, Pile (M.singleton (PlayerMarker player) 3)) | player <- ps]

initDice :: Map CantStopLocation Counter
initDice = M.fromList [(die, Counter Nothing (1,6)) | die <- theDice]

diceSums :: CantStopGameObjects -> Maybe [(Cnt Int,Cnt Int)]
diceSums objs = sums where
     cs = view #counters objs
     ds = (view #val . (cs M.!) <$> theDice) :: [Maybe (Cnt Int)]
     sums = case sequence ds of
             Nothing -> Just [] :: Maybe [(Cnt Int, Cnt Int)]
             Just ds' -> mapM (bimapM (`getSum` objs) (`getSum` objs)) perms
                  where
                      perms = [((DieOne,DieTwo),(DieThree,DieFour)),((DieOne,DieThree),(DieTwo,DieFour)),((DieOne,DieFour), (DieTwo,DieThree))]
                      getSum :: (CantStopLocation, CantStopLocation) -> CantStopGameObjects -> Maybe (Cnt Int)
                      getSum (d, d') gobj = (+) <$> (view #val =<< preview (#counters . ix d ) gobj )
                                                <*> (view #val =<< preview (#counters . ix d' ) gobj )

-- all the ways to sum a roll
-- works for any even number of dice, i.e. overengineered
sumsOfPairs :: forall a. Num a => [a] -> [[a]]
sumsOfPairs as = sumPairs (mkPairs as) where
    -- pigworker here https://stackoverflow.com/questions/12869097/splitting-list-into-a-list-of-possible-tuples
    sumPairs :: Num a => [[(a,a)]] -> [[a]]
    sumPairs = fmap (fmap (uncurry (+)))
    mkPairs :: [a] -> [[(a,a)]]
    mkPairs (a:as) = [(a,b):bs | (preb,b,postb) <- zippers as, bs <- mkPairs (preb++postb) ]
    mkPairs [] = [[]]
    zippers :: [a] -> [([a],a,[a])]
    zippers as = go as [] where
        go :: [a] -> [([a],a,[a])] -> [([a],a,[a])]
        -- assume first list is in 'reverse' order
        -- [1,2,3,4] -> [([],1,[2,3,4]), ([1],2,[3,4]), ([2,1],3,[4]), ([3,2,1],4,[])
        go (x:xs) [] = go xs [([],x,xs)]
        go (x:xs) (y@(y0,y1,_):ys) = go xs ((y1:y0, x, xs) : (y:ys))
        go [] ys = ys


data CantStopPlayName = MoveUp TrackName | Stop
data CantStopPhaseName = Roll | PlayerChoice Player deriving (Eq, Ord, Show, Generic)

type CantStopPlay = Play CantStopPlayName CantStopLocation CantStopResource CantStopPhaseName


type CantStopPhase = Phase CantStopPhaseName CantStopLocation CantStopResource CantStopPlayName

type CantStopCondition val = Condition CantStopLocation CantStopResource CantStopPhaseName CantStopPlayName val
type CantStopGame = Game CantStopLocation CantStopResource CantStopPhaseName



doNothing :: p1 -> p2 -> [a]
doNothing _ _ = []

rollAction :: Game CantStopLocation r p2 -> Game CantStopLocation r p2
rollAction = compose [fst . roll die | die <- theDice]





