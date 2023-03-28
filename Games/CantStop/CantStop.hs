{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE ConstraintKinds #-}

module CantStop
    where

import Control.Lens (to)
import Count ( histogramF, Cnt )
import Data.Bitraversable ( Bitraversable(bitraverse) )
import Data.Finitary (Finitary, inhabitants)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe, mapMaybe, isJust, fromMaybe)
import qualified Data.Sequence as Seq
import FinitaryMap (FTMap (..))
import GHC.Generics (Generic)
import GameE
import Location (Counter (..), Counters, GameObjects (..), LocationShape (..), Locations, counters, d6, findResourceWithin, inventory, listAllF, howMany')
import Data.Set (Set)
import qualified Data.Set as S
import Game.Player
import qualified Data.Set as Set
import Game.Options (Options (..), Legality (..), mustNotElse, buildOptions)
import Data.List (find)
import Effectful (Eff)
import GameNode
import Util (graphMapM)
import Visibility (VisibilityMap (..), allVisible)


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
data CantStopPhaseName = Turn Player deriving (Eq, Ord, Show, Generic)
type CantStopPhase = Phase CantStopPhaseName CantStopLocation CantStopCounterName CantStopResource PlayName Issue
type CantStopAction = GameAction CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName

-- type CantStopGame = Game CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName PlayName CantStopTurns Player

type CantStopGameState = GameState CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName PlayName Issue
type CantStopGame = Game  CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName PlayName Issue
type CantStopGameNode = GameNode CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName PlayName Issue
-- type CantStopGetOptions = GetOptions CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName PlayName Issue
type Observe es = ObserveGame CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName PlayName Issue es


rollNodes :: Observe es => Eff es [CantStopGameNode]
rollNodes = pure $ mkActionNode . RollCounter <$> inhabitants

currentPlayer :: CantStopPhaseName ->Player
currentPlayer (Turn p) = p

chooseMove :: Observe es => Player -> Eff es [CantStopGameNode]
chooseMove p = (: []) . mkGetOptionsNode p <$> legalMoves p
  where
    legalMoves ::Observe  es =>  Player -> Eff es (Options PlayName Issue)
    legalMoves p = buildOptions checkMove (Stop p) (fmap (makeMove p) <$> diceVals)
    makeMove :: Player -> (Cnt Int, Cnt Int) -> PlayName
    makeMove p (x, y) = Move p (diceToTrack x) (diceToTrack y)
    checkMove' :: Observe es => Player -> TrackName -> Eff es (Legality Issue)
    checkMove' p track = do
        noMarkersLeft <- flip mustNotElse ThreeTempMarkersOut <$> BoxTop `doesNotHave` TemporaryMarker
        trackWon <- flip mustNotElse TrackCompleted . isJust <$> trackWinner track
        atTop <- flip mustNotElse AtTop <$> maxSpot track `has` PlayerMarker p
        return (noMarkersLeft <> trackWon <> atTop)
    checkMove ::  Observe es => PlayName -> Eff es (Legality Issue)
    checkMove (Move p s t) = do
        check_s <- checkMove' p s
        check_t <- checkMove' p t
        -- Legality is defined by (essentially) the First monoid. What is this alternative structure?
        if check_s == Legal || check_t == Legal
        then return Legal
        else return (check_s <> check_t)
    -- TODO: is this true?
    checkMove _ = pure Legal

stopOrGoNode :: Observe es => Player -> Eff es [CantStopGameNode]
stopOrGoNode p =  (: []) . mkGetOptionsNode p <$> stopOrGo p
    where
        stopOrGo :: Observe es => Player -> Eff es (Options PlayName i)
        stopOrGo p =  return (Options (NE.fromList [Stop p, DontStop p]) mempty)

diceVals :: Observe es => Eff es [(Cnt Int, Cnt Int)]
diceVals = mapM (bitraverse makeSum makeSum) (mkPairs theDiceL)
  where
    makeSum :: Observe  es => (CantStopCounterName, CantStopCounterName) -> Eff es (Cnt Int)
    makeSum (c, c') = useGameState (counterVal c)  + useGameState (counterVal c')
    mkPairs :: (a, a, a, a) -> [((a, a), (a, a))]
    mkPairs (x, y, w, z) =
      let pairs1 = [((x, y), (w, z)), ((x, w), (y, z)), ((x, z), (y, w))]
          -- pairs2 = fmap swap pairs1
       in pairs1 -- ++ pairs2

advancePlayer :: Observe es => Eff es [CantStopGameNode]
advancePlayer = do
    players <- useGameState #players
    currentPlayer <- useGameState (#currentPhase . to currentPlayer)
    let nextp = unsafeNextCyclic currentPlayer players
    return [mkActionNode (ChangePhase (Turn nextp))]
    where
        unsafeNextCyclic :: Player -> Set Player -> Player
        unsafeNextCyclic player players = go player (S.toList players) (S.toList players) where
            go p (p':p'':ps) allps = if p == p' then p'' else go p (p'':ps) allps
            go _ [_] allps = head allps

cantStopPhases :: CantStopPhaseName -> CantStopPhase
cantStopPhases (Turn p) =  Phase
    { name = Turn p,
      seedNodes = [rollNodes, chooseMove p]
    }


initGameState :: Int -> CantStopGameState
initGameState numPlayers =
    let players = Set.fromList (fmap Player [1 ..fromIntegral numPlayers])
    in
  GameState
      { players = players,
      objects = initGameObjects players,
      currentPhase = Turn (head (S.toList players)),
      phases = cantStopPhases
    }

csVisibility :: VisibilityMap CantStopLocation CantStopCounterName
csVisibility = allVisible

csRunPlay :: Observe es => PlayName -> [Eff es [CantStopGameNode]]
csRunPlay (Move pl s t) = [moveMarkerBy pl s 1
                          , moveMarkerBy pl t 1
                          , stopOrGoNode pl]
csRunPlay (Stop pl) = [
    resolveMarkers pl
                      , clearWonTracks
                      , checkWinner
                      , advancePlayer
                      ]
csRunPlay (DontStop p) =
    [rollNodes, chooseMove p]

returnMarker :: CantStopLocation -> CantStopResource -> GameAction CantStopLocation cn CantStopResource ph
returnMarker n (PlayerMarker p') = MkTransfer n (PlayerStuff p') (PlayerMarker p')
returnMarker n TemporaryMarker = MkTransfer n BoxTop TemporaryMarker

resolveMarkers :: Observe es => Player -> Eff es [CantStopGameNode]
resolveMarkers pl = do
  tempMarkerLocs <- findResourceWithin' TemporaryMarker allSpots
  playerMarkerLocs <- findResourceWithin' (PlayerMarker pl) allSpots
  return (concatMap (resolveMarker' pl playerMarkerLocs) tempMarkerLocs)
  where
    resolveMarker' :: Player -> [CantStopLocation] -> CantStopLocation -> [CantStopGameNode]
    resolveMarker' player playerMarkers newLoc@(TrackSpot track _) =
        let currentPosition = fromMaybe (PlayerStuff player)
                              . find (\(TrackSpot track' _) -> track' == track)
                              $ playerMarkers
         in mkMoveNode player <$>
             [MkTransfer currentPosition newLoc (PlayerMarker player),
              returnMarker newLoc TemporaryMarker]
    resolveMarker' _ _ _ = error "resolveMarker' called with newLoc not a TrackSpot!"

clearWonTracks :: Observe es => Eff es [CantStopGameNode]
clearWonTracks = do
    trackWinnerList <- trackWinners
    concat <$> M.traverseWithKey moveOtherMarkers trackWinnerList
        where
            moveOtherMarkers :: Observe es => TrackName -> Player -> Eff es [CantStopGameNode]
            moveOtherMarkers track winningP = do
                locs <- useGameState (#objects . #locations)
                let
                -- TODO: could still simplify
                  getMarkersToMove :: CantStopLocation -> [CantStopResource]
                  getMarkersToMove n = listAllF n locs (/= PlayerMarker winningP)
                  mkReturns track = returnMarker track <$> getMarkersToMove track
                pure $ mkActionNode <$> concatMap  mkReturns (trackSlots track)


checkWinner :: Observe es => Eff es [CantStopGameNode]
checkWinner = do
  trackMap <- trackWinners
  let playerHistogram = histogramF trackMap
  let winners = M.keys . M.filter (>= 3) $ playerHistogram
  if not (null winners)
  then return [mkActionNode EndGame]
  else return [mkActionNode DoNothing]

trackWinner :: Observe es => TrackName -> Eff es (Maybe Player)
trackWinner track = do
  things <- useGameState (location (maxSpot track))
  return $ listToMaybe . mapMaybe markerOwner . M.keys . M.filter (> 0) $ inventory things

trackWinners :: Observe es => Eff es (Map TrackName Player)
trackWinners = graphMapM trackWinner inhabitants

moveMarkerBy :: Observe es => Player -> TrackName -> Int -> Eff es [CantStopGameNode]
moveMarkerBy pl s amt = do
  let slots = trackSlots s
  tempMarkerLocs <- findResourceWithin' TemporaryMarker (NE.toList slots)
  playerMarkerLocs <- findResourceWithin' (PlayerMarker pl) (NE.toList slots)
  -- TODO: rewrite with Alternative
  return $ case (listToMaybe tempMarkerLocs, listToMaybe playerMarkerLocs) of
    -- if the TemporaryMarker is out, move 2 (or 1 if there's not enough space)
    (Just slot@(TrackSpot s height), _) ->
      let gap = min (getHeight (maxSlot s) - getHeight height) amt -- this is >0, otherwise move would be illegal
          nextSlot = TrackSpot s (height <+ gap) -- target slot
      in [mkActionNode (MkTransfer slot nextSlot TemporaryMarker)]
    -- if you can't find the temporary marker, look for the player marker on that track
    -- if you find it, place the TemporaryMarker
    (Nothing, Just (TrackSpot s height)) ->
      let nextSlot = TrackSpot s (height <+ amt)
       in [mkActionNode (MkTransfer BoxTop nextSlot TemporaryMarker)]
    -- if you don't find it, just place the TemporaryMarker
    (Nothing, Nothing) -> [mkActionNode (MkTransfer BoxTop (TrackSpot s (toEnum (amt-1))) TemporaryMarker)]

    (_,_) -> [] -- This is an interesting case -- as long as Locations are all one type there will have to be lame patterns like this
    -- guess you can't stop someone from writing bad rules!


runEffCSNodes effNodes = do
    let gs = initGameState 3
    runEffNodesAgainstState gs csRunPlay effNodes

runCSNodes = runNodesAgainstState (initGameState 3) csRunPlay allVisible

moreInterestingGameState = runCSNodes [mkActionNode (MkTransfer (PlayerStuff (Player 2)) (TrackSpot Six HThree) (PlayerMarker (Player 2))),
                                      mkActionNode (MkTransfer (PlayerStuff (Player 1)) (TrackSpot Two HOne) (PlayerMarker (Player 1))),
                                      mkActionNode (MkTransfer (PlayerStuff (Player 3)) (TrackSpot Ten HTwo) (PlayerMarker (Player 3)))]



--- Helpers ---
(<+) :: Enum a => a -> Int -> a
a <+ i
        | i > 0  = succ a <+ (i-1)
        | i < 0  = pred a <+ (i+1)
        | otherwise = a

findResourceWithin' :: Observe es => CantStopResource -> [CantStopLocation] -> Eff es[CantStopLocation]
findResourceWithin' r locationNames = do
    locs <- useGameState (#objects . #locations)
    return $ findResourceWithin r locationNames locs

mkMoveNode :: Player -> GameAction l cn r ph -> GameNode l cn r ph pl i
mkMoveNode p act = GameNode (Left act) (Just p)

howManyAt :: Observe es => CantStopLocation -> CantStopResource -> Eff es (Cnt Int)
howManyAt l r = flip howMany' r <$> useGameState (location l)

has :: Observe es => CantStopLocation -> CantStopResource -> Eff es Bool
has l r = (> 0) <$> howManyAt l r
doesNotHave :: Observe es => CantStopLocation -> CantStopResource -> Eff es Bool
doesNotHave l r = not <$> has l r


