{-# LANGUAGE DeriveAnyClass #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module CantStop where

import Control.Lens (use, uses)
import Control.Monad.Random (mkStdGen)
import Control.Monad.State.Lazy
import Count
import Data.Bitraversable
import Data.Finitary (Finitary, inhabitants)
-- import qualified FinitaryMap as FT

-- import qualified FinitaryMap as FT
-- import qualified FinitaryMap as FT

-- import qualified FinitaryMap as FT

import Data.Foldable (maximumBy)
import Data.List (find, group, sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import qualified Data.Sequence as Seq
import Data.Tuple (swap)
import qualified Defaultable.Map as D
import FinitaryMap (FTMap (..), ftAt, (!!!))
import GHC.Base (liftA2)
import GHC.Generics
import Game
import Game.Condition
import Game.Player (Player)
import Location (Counter (..), Counters, GameObjects (..), LocationShape (..), Locations, counters, d6, findResourceWithin, inventory)
import Util

data TrackName = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Eleven | Twelve
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Finitary)

trackNum :: TrackName -> Int
trackNum t = fromEnum t + 2

maxSlot :: TrackName -> Int
maxSlot t = trackNum t + 1

type TrackHeight = Int

data CantStopResource = PlayerMarker Player | TemporaryMarker deriving (Eq, Ord, Show, Generic)

data CantStopLocation
  = TrackSpot TrackName TrackHeight
  | BoxTop
  | PlayerStuff Player
  deriving (Eq, Ord, Show, Generic)

-- TODO: should be non-empty
trackSlots :: TrackName -> NonEmpty CantStopLocation
trackSlots track = NE.fromList $ TrackSpot track <$> [1 .. maxSlot track]

data CantStopCounterName = DieOne | DieTwo | DieThree | DieFour
  deriving (Eq, Ord, Show, Generic, Enum)
  deriving anyclass (Finitary)

type CantStopLocations = Locations CantStopLocation CantStopResource

type CantStopCounters = Counters CantStopCounterName

type CantStopGameObjects = GameObjects CantStopLocation CantStopCounterName CantStopResource

allSpots :: [CantStopLocation]
allSpots = [TrackSpot name height | name <- inhabitants, height <- [1 .. maxSlot name]]

theDiceL :: (CantStopCounterName, CantStopCounterName, CantStopCounterName, CantStopCounterName)
theDiceL = (DieOne, DieTwo, DieThree, DieFour)

--
initLocations' :: CantStopLocation -> LocationShape CantStopResource
initLocations' (TrackSpot _ _) = Deck Seq.empty
initLocations' BoxTop = Pile (D.singleton (TemporaryMarker, 3))
initLocations' (PlayerStuff player) = Pile (D.singleton (PlayerMarker player, 11))

initLocations :: CantStopLocations -- FTMap CantStopLocation (LocationShape CantStopResource)
initLocations = FTMap initLocations'

initDice' :: CantStopCounterName -> Counter
initDice' = const d6

initDice :: CantStopCounters
initDice = FTMap initDice'

initGameObjects :: CantStopGameObjects
initGameObjects =
  GameObjects
    { locations = initLocations,
      counters = initDice
    }

data CantStopTriggers deriving (Eq, Ord, Show, Generic)

data MoveArity = TwoValueMove TrackName TrackName | OneValueMove TrackName deriving (Eq, Ord, Show, Generic)

data CantStopPlayName = Move Player MoveArity | Stop Player deriving (Eq, Ord, Show, Generic)

data CantStopPhaseName = Turn Player | Winner Player deriving (Eq, Ord, Show, Generic)

type CantStopTurns = Int

type CantStopPhase = Phase CantStopPhaseName CantStopLocation CantStopCounterName CantStopResource CantStopPlayName CantStopTurns CantStopTriggers

type CantStopAction = GameAction CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName

-- type CantStopCondition val = Condition CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName CantStopTurns CantStopTriggers val

type CantStopGame = Game CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName CantStopTurns CantStopTriggers

type CantStopGameNode = GameNode CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName CantStopTurns CantStopTriggers

type CantStopChoice = Choice CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName CantStopTurns CantStopTriggers

type CantStopGameS = State CantStopGame

type CantStopActionS = CantStopGameS CantStopAction

rollDice' :: [CantStopAction]
rollDice' = RollCounter <$> (inhabitants :: [CantStopCounterName])

playerTurn :: Player -> CantStopPhase
playerTurn p =
  Phase
    { name = Turn p,
      seedNodes = [rollNode, chooseToRollOrStopNode p]
    }

winnerPhase :: Player -> CantStopPhase
winnerPhase p =
  Phase
    { name = Winner p,
      seedNodes = [mkActionNode EndGame]
    }

-- good spot for a helper, esp w/ `parents`
rollNode :: CantStopGameNode
rollNode = mkActionNodeL rollDice'

chooseToRollOrStopNode :: Player -> CantStopGameNode
chooseToRollOrStopNode p = mkChoiceNode p (chooseToRollOrStop p)

-- andThen :: (Applicative f, Semigroup a) => f [a] -> a -> f [a]
-- andThen xs y = liftA2 (<>) xs (pure [y])
andThen :: GameS l cn r ph pl t tn [x] -> GameS l cn r ph pl t tn x -> GameS l cn r ph pl t tn [x]
andThen xs y = liftA2 (<>) xs (fmap (: []) y)

also = andThen

chooseToRollOrStop :: Player -> CantStopChoice
chooseToRollOrStop p = legalRolls p `also` pure (Stop p)
  where
    legalRolls :: Player -> CantStopGameS [CantStopPlayName]
    legalRolls p = fmap (fmap (makeRoll p)) diceVals
    makeRoll :: Player -> (Cnt Int, Cnt Int) -> CantStopPlayName
    makeRoll p (x, y) =
      if x == y
        then Move p (OneValueMove (coerceEnum x))
        else Move p (TwoValueMove (coerceEnum x) (coerceEnum y))

diceVals :: CantStopGameS [(Cnt Int, Cnt Int)]
diceVals = runCondition $ mapM (bitraverse makeSum makeSum) (mkPairs theDiceL)
  where
    makeSum (c, c') = cCounterVal c + cCounterVal c'
    mkPairs :: (a, a, a, a) -> [((a, a), (a, a))]
    mkPairs (x, y, w, z) =
      let pairs1 = [((x, y), (w, z)), ((x, w), (y, z)), ((x, z), (y, w))]
          pairs2 = fmap swap pairs1
       in pairs1 ++ pairs2

advancePlayer :: CantStopGameS Player
advancePlayer = do
  ps <- use #players
  p <- use #activePlayer
  case p of
    Nothing -> return $ head ps
    Just p' -> return (unsafeNextCyclic p' ps) -- Lame

initGame :: [Player] -> CantStopGame
initGame ps =
  Game
    { players = ps,
      objects = initGameObjects,
      runPlay = csRunPlay,
      randGen = mkStdGen 100,
      chooser = undefined,
      activePlayer = Nothing,
      turnNumber = 1,
      phase = Turn (head ps)
    }

-- Where do we decide legality?
-- in legalMoves / chooser in Game. In other words,
-- pure logic (incl. legality) -> impure choice -> pure logic (resolution)
-- this is in stage 3. So assume move is legal.
-- Could validate the moves instead. Something to consider for giant type cleanup :)

-- Assumes the move is legal. So don't check for:
--   - track closed
--   - marker already at top
-- still need to check for correct distance to move
csRunPlay :: CantStopPlayName -> CantStopGameS [CantStopGameNode]
csRunPlay (Move pl (OneValueMove s)) = moveMarkerBy pl s 2
csRunPlay (Move pl (TwoValueMove s t)) = (++) <$> moveMarkerBy pl s 1 <*> moveMarkerBy pl t 1
csRunPlay (Stop pl) = do
  ps <- use #players
  let nextp = unsafeNextCyclic pl ps
  resolveMarkers pl
    `andThen` checkWinner
    `andThen` mkActionNodeS (ChangePhase (Turn nextp))

checkWinner :: CantStopGameS CantStopGameNode
checkWinner = do
  trackMap <- trackWinners
  let playerHistogram = histogramF . fmap snd $ M.toList trackMap
  let winners = M.keys . M.filter (>= 3) . D.toMap $ playerHistogram
  return $ if not (null winners) then mkActionNode EndGame else mkActionNode DoNothing

trackWinner :: TrackName -> CantStopGameS (Maybe Player)
trackWinner track = do
  things <- use (#objects . #locations . ftAt (TrackSpot track (maxSlot track)))
  let getPlayer res = case res of
        PlayerMarker p -> Just p
        _ -> Nothing
  return $ listToMaybe . mapMaybe getPlayer . M.keys . M.filter (> 0) $ D.toMap (inventory things)

trackWinners :: CantStopGameS (Map TrackName Player)
trackWinners = do
  let winners = mconcat $ fmap (\track -> M.singleton track (trackWinner track)) inhabitants
  fmap unMaybeMap . sequence $ winners
  where
    unMaybeMap :: Ord k => Map k (Maybe a) -> Map k a
    unMaybeMap = M.fromList . mapMaybe sequence . M.toList

moveMarkerBy :: Player -> TrackName -> Int -> CantStopGameS [CantStopGameNode]
moveMarkerBy pl s amt = do
  let slots = trackSlots s
  tempMarkerLocs <- uses (#objects . #locations) (findResourceWithin TemporaryMarker (NE.toList slots))
  playerMarkerLocs <- uses (#objects . #locations) (findResourceWithin (PlayerMarker pl) (NE.toList slots))
  case (listToMaybe tempMarkerLocs, listToMaybe playerMarkerLocs) of
    -- if the TemporaryMarker is out, move 2 (or 1 if there's not enough space)
    (Just slot@(TrackSpot s height), _) ->
      let gap = min (maxSlot s - height) amt -- this is >0, otherwise move would be illegal
          nextSlot = TrackSpot s (height + gap) -- target slot
       in return [mkMoveNode pl (MkTransfer slot nextSlot TemporaryMarker)]
    -- if you can't find the temporary marker, look for the player marker on that track
    -- if you find it, place the TemporaryMarker
    (Nothing, Just (TrackSpot s height)) ->
      let nextSlot = TrackSpot s (height + (amt - 1)) -- subtract 1 because you use one move to line up
       in return [mkMoveNode pl (MkTransfer BoxTop nextSlot TemporaryMarker)]
    -- if you don't find it, just place the TemporaryMarker
    -- don't subtract here -- moving "from 0."
    (Nothing, Nothing) -> return [mkMoveNode pl (MkTransfer BoxTop (TrackSpot s amt) TemporaryMarker)]

mkMoveNode :: Player -> CantStopAction -> CantStopGameNode
mkMoveNode p action = GameNode (Right [action]) Nothing (Just p) []

-- ODO: knock off other markers
-- no, leave them on. Let UI/legality-checking handle them
resolveMarkers :: Player -> CantStopGameS [CantStopGameNode]
resolveMarkers pl = do
  tempMarkerLocs <- uses (#objects . #locations) (findResourceWithin TemporaryMarker allSpots)
  playerMarkerLocs <- uses (#objects . #locations) (findResourceWithin (PlayerMarker pl) allSpots)
  let tempMarkerPairs = fmap locToNameHeight tempMarkerLocs
  let playerMarkerPairs = fmap locToNameHeight playerMarkerLocs
  return (concatMap (resolveMarker' pl playerMarkerPairs) tempMarkerPairs)
  where
    resolveMarker' :: Player -> [(TrackName, TrackHeight)] -> (TrackName, TrackHeight) -> [CantStopGameNode]
    resolveMarker' player playerMarkers (nm, newHeight) = case lookup nm playerMarkers of
      Just h ->
        [ mkMoveNode player (MkTransfer (TrackSpot nm h) (TrackSpot nm newHeight) (PlayerMarker player)),
          mkMoveNode player (MkTransfer (TrackSpot nm newHeight) BoxTop TemporaryMarker)
        ]
      Nothing ->
        [ mkMoveNode player (MkTransfer BoxTop (TrackSpot nm newHeight) (PlayerMarker pl)),
          mkMoveNode player (MkTransfer (TrackSpot nm newHeight) BoxTop TemporaryMarker)
        ]

    locToNameHeight (TrackSpot nm h) = (nm, h)
    locToNameHeight _ = error "applied to non-track"
