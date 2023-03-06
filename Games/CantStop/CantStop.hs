{-# LANGUAGE DeriveAnyClass #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TupleSections #-}

module CantStop where

import Control.Lens ((^.), view, over, to)
import Count
import Data.Bitraversable
import Data.Finitary (Finitary, inhabitants)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe)
import qualified Data.Sequence as Seq
import qualified Defaultable.Map as D
import FinitaryMap (FTMap (..), ftAt)
import GHC.Base (liftA2)
import GHC.Generics (Generic)
import GameE
import Location (Counter (..), Counters, GameObjects (..), LocationShape (..), Locations, counters, d6, findResourceWithin, inventory, listAllF, has')
import Data.Set (Set)
import qualified Data.Set as S
import Game.Condition (cCounterVal)
import Effectful.Reader.Static (asks)
import Game.Player
import qualified Data.Set as Set
import Effectful (inject)
import qualified Effectful.Reader.Static as R
import qualified Data.Text as T
import Effectful.Log (logInfo)
import Game.Options (Options (..), Legality (..))

data TrackName = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Eleven | Twelve
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Finitary)

data TrackHeight = HOne | HTwo | HThree | HFour | HFive | HSix | HSeven | HEight | HNine | HTen deriving (Eq, Ord, Show, Enum, Generic)
instance Finitary TrackHeight

trackNum :: TrackName -> Int
trackNum t = fromEnum t + 2
maxSlot :: TrackName -> TrackHeight
maxSlot t = if t <= Seven
            then toEnum (trackNum t)
            else toEnum (14 - trackNum t)
getHeight :: TrackHeight -> Int
getHeight h = fromEnum h + 1

data CantStopResource = PlayerMarker Player | TemporaryMarker deriving (Eq, Ord, Show, Generic)

data CantStopLocation
  = TrackSpot TrackName TrackHeight
  | BoxTop
  | PlayerStuff Player
  deriving (Eq, Ord, Show, Generic)

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
data MoveArity = TwoValueMove TrackName TrackName | OneValueMove TrackName deriving (Eq, Ord, Show, Generic)

data PlayName = Move Player MoveArity | Stop Player | DontStop Player deriving (Eq, Ord, Show, Generic)

data CantStopPhaseName = Turn Player deriving (Eq, Ord, Show, Generic)

type CantStopPhase = Phase CantStopPhaseName CantStopLocation CantStopCounterName CantStopResource PlayName Issue

type CantStopAction = GameAction CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName

-- type CantStopGame = Game CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName PlayName CantStopTurns Player

type CantStopGameData = GameData CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName
type CantStopGameRules = GameRules CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName PlayName Issue

type CantStopGameNode = GameNode CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName PlayName Issue
type CantStopGetOptions = GetOptions CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName PlayName Issue

type Observe = ObserveGame CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName Issue

type ObserveWithRules = ObserveRulesGame  CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName PlayName Issue

type Modify = ModifyGame CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName



rollDice' :: [CantStopAction]
rollDice' = RollCounter <$> (inhabitants :: [CantStopCounterName])

rollNodes :: [CantStopGameNode]
rollNodes = mkActionNode <$> rollDice'

playerTurn :: Player -> CantStopPhase
playerTurn p =
  Phase
    { name = Turn p,
      seedNodes = pure rollNodes <> chooseMove p
    }

currentPlayer :: CantStopPhaseName ->Player
currentPlayer (Turn p) = p


andThen :: (Applicative f) => f [a] -> f a -> f [a]
andThen xs y = liftA2 (<>) xs (fmap (:[]) y)


graph :: (a -> b) -> (a -> (a,b))
graph f a = (a, f a)

graphM :: Functor m => (a -> m b) -> (a -> m (a,b))
graphM f a = fmap (a,) (f a)

-- Should return [GetOptions of move]
-- Then GetOptions of move leads to GetOptions of stop / go
-- TODO: double-check that AtTop functions correctly!
chooseMove :: Player -> Observe [CantStopGameNode]
chooseMove p =
    return [mkGetOptionsNode p (legalMoves p)]
  where
    legalMoves :: Player -> Observe (Options PlayName Issue)
    legalMoves p = do
        plays <- fmap (makeMove p) <$> diceVals
        playsWithLegality <- traverse (graphM checkMove) plays
        let legalMoves = fmap fst . filter ((== Legal) . snd) $ playsWithLegality
        let illegalMoves = filter ((/= Legal) . snd) playsWithLegality
        return $ Options (fromMaybe (NE.singleton (Stop p)) (NE.nonEmpty legalMoves)) illegalMoves
    makeMove :: Player -> (Cnt Int, Cnt Int) -> PlayName
    makeMove p (x, y) = if x == y
                        then Move p (OneValueMove (diceToTrack x))
                        else Move p (TwoValueMove (diceToTrack x) (diceToTrack y))
    diceToTrack :: Cnt Int -> TrackName
    diceToTrack x = toEnum . fromEnum $ (x - 2)
    -- TODO: shows that OneValue and TwoValue should be consolidated
    checkMove :: PlayName -> Observe (Legality Issue)
    checkMove (Move p (OneValueMove s)) = do
        hasWinner <- maybe Legal  (const (Illegal TrackCompleted)) . M.lookup s <$> trackWinners
        numTempLeft <- fromMaybe 0 . M.lookup TemporaryMarker . inventory <$> R.asks (view (#objects . #locations . ftAt BoxTop))
        let noMarkersLeft = if numTempLeft == 0 then Illegal ThreeTempMarkersOut else Legal
        atTop' <- flip has' (PlayerMarker p) <$> R.asks (view (#objects . #locations . ftAt (TrackSpot s (maxSlot s))))
        let atTop = if atTop' then Illegal AtTop else Legal
        return (hasWinner <> noMarkersLeft <> atTop)
    checkMove (Move p (TwoValueMove s t)) =
        checkMove (Move p (OneValueMove s)) <> checkMove (Move p (OneValueMove t))
    checkMove (Stop _) = pure Legal
    -- TODO: is this true?
    checkMove (DontStop _) = pure Legal


stopOrGo :: Player -> CantStopGetOptions
stopOrGo p = return (Options (NE.fromList [Stop p, DontStop p]) [])

stopOrGoNode :: Player -> CantStopGameNode
stopOrGoNode p = mkGetOptionsNode p (stopOrGo p)

diceVals :: Observe [(Cnt Int, Cnt Int)]
diceVals = mapM (bitraverse makeSum makeSum) (mkPairs theDiceL)
  where
    makeSum (c, c') = cCounterVal c + cCounterVal c'
    mkPairs :: (a, a, a, a) -> [((a, a), (a, a))]
    mkPairs (x, y, w, z) =
      let pairs1 = [((x, y), (w, z)), ((x, w), (y, z)), ((x, z), (y, w))]
          -- pairs2 = fmap swap pairs1
       in pairs1 -- ++ pairs2



advancePlayer :: ObserveWithRules CantStopGameNode
advancePlayer = do
    players <- observe #players
    currentPlayer <- observe (#currentPhase . to currentPlayer)
    let nextp = unsafeNextCyclic currentPlayer players
    return (mkActionNode (ChangePhase (Turn nextp)))
    where
        unsafeNextCyclic :: Player -> Set Player -> Player
        unsafeNextCyclic player players = go player (S.toList players) (S.toList players) where
            go p (p':p'':ps) allps = if p == p' then p'' else go p (p'':ps) allps
            go _ [_] allps = head allps 

cantStopPhases :: CantStopPhaseName -> CantStopPhase
cantStopPhases (Turn p) = playerTurn p

initGameData :: Int -> CantStopGameData
initGameData numPlayers =
    let players = Set.fromList . fmap Player $ take numPlayers inhabitants
    in
  GameData
      { players = players,
      objects = initGameObjects players,
      currentPhase = Turn (head (S.toList players))
    }

gameRules :: CantStopGameRules
gameRules = GameRules {
    phases = cantStopPhases,
    runPlay = csRunPlay
                      }





-- Where do we decide legality?

-- Assumes the move is legal. So don't check for:
--   - track closed
--   - marker already at top
-- still need to compute correct distance to move
csRunPlay :: PlayName -> ObserveWithRules [CantStopGameNode]
csRunPlay (Move pl (OneValueMove s)) = inject (moveMarkerBy pl s 2 `andThen` pure (stopOrGoNode pl))
csRunPlay (Move pl (TwoValueMove s t)) = inject (moveMarkerBy pl s 1 <> moveMarkerBy pl t 1) `andThen` pure (stopOrGoNode pl)
csRunPlay (Stop pl) = do
  inject (resolveMarkers pl <> clearWonTracks
    `andThen` checkWinner)
    `andThen` advancePlayer -- TODO: awkward -- why not just all Observe?
csRunPlay (DontStop _) = do
    gd <- R.ask :: ObserveWithRules CantStopGameData
    case view #currentPhase gd of
      Turn p ->  inject $ pure rollNodes <> chooseMove p

clearWonTracks :: Observe [CantStopGameNode]
clearWonTracks = do
    trackWinnerList <- trackWinners
    concat <$> M.traverseWithKey moveOtherMarkers trackWinnerList
        where
            moveOtherMarkers :: TrackName -> Player -> Observe [CantStopGameNode]
            moveOtherMarkers track winningP = do
                locs <- asks (view (#objects . #locations))
                let getMarkersToMove n = listAllF n locs (/= PlayerMarker winningP)
                -- TODO: improve
                let test = concat . NE.toList $ (\n -> mkTransferBack n <$> getMarkersToMove n) <$> trackSlots track
                return $ fmap mkActionNode test
                    where
                        mkTransferBack n (PlayerMarker p') = MkTransfer n (PlayerStuff p') (PlayerMarker p')
                        mkTransferBack n TemporaryMarker = MkTransfer n BoxTop TemporaryMarker



checkWinner :: Observe CantStopGameNode
checkWinner = do
  trackMap <- trackWinners
  let playerHistogram = histogramF trackMap
  let winners = M.keys . M.filter (>= 3) $ playerHistogram
  if not (null winners) then logInfo (T.pack "We have a winner!") (show winners ++ show playerHistogram) >> return (mkActionNode EndGame) else return (mkActionNode DoNothing)

trackWinner :: TrackName -> Observe (Maybe Player)
trackWinner track = do
  things <- asks (view (#objects . #locations . ftAt (TrackSpot track (maxSlot track))))
  let getPlayer res = case res of
        PlayerMarker p -> Just p
        _ -> Nothing
  return $ listToMaybe . mapMaybe getPlayer . M.keys . M.filter (> 0) $ D.toMap (inventory things)

-- TODO: simplify
trackWinners :: Observe (Map TrackName Player)
trackWinners = fmap (M.mapMaybe id) . sequence . mconcat $ fmap (\track -> M.singleton track (trackWinner track)) inhabitants

moveMarkerBy :: Player -> TrackName -> Int -> Observe [CantStopGameNode]
moveMarkerBy pl s amt = do
  let slots = trackSlots s
  tempMarkerLocs <- findResourceWithin TemporaryMarker (NE.toList slots) <$> asks (view (#objects . #locations))
  playerMarkerLocs <- findResourceWithin (PlayerMarker pl) (NE.toList slots) <$> asks (view (#objects . #locations))
  -- TODO: rewrite with Alternative
  case (listToMaybe tempMarkerLocs, listToMaybe playerMarkerLocs) of
    -- if the TemporaryMarker is out, move 2 (or 1 if there's not enough space)
    (Just slot@(TrackSpot s height), _) ->
      let gap = min (getHeight (maxSlot s) - getHeight height) amt -- this is >0, otherwise move would be illegal
          nextSlot = TrackSpot s (height <+ gap) -- target slot
       in return [mkMoveNode pl (MkTransfer slot nextSlot TemporaryMarker)]
    -- if you can't find the temporary marker, look for the player marker on that track
    -- if you find it, place the TemporaryMarker
    (Nothing, Just slot@(TrackSpot s height)) ->
      let nextSlot = TrackSpot s (height <+ amt)
       in return [mkMoveNode pl (MkTransfer BoxTop nextSlot TemporaryMarker)]
    -- if you don't find it, just place the TemporaryMarker
    (Nothing, Nothing) -> return [mkMoveNode pl (MkTransfer BoxTop (TrackSpot s (toEnum (amt-1))) TemporaryMarker)]
    (_,_) -> return [] -- This is an interesting case -- as long as Locations are all one type there will have to be lame patterns like this
    -- guess you can't stop someone from writing bad rules!

mkMoveNode :: Player -> CantStopAction -> CantStopGameNode
mkMoveNode p action = GameNode (Left action) (Just p)

resolveMarkers :: Player -> Observe [CantStopGameNode]
resolveMarkers pl = do
  tempMarkerLocs <- findResourceWithin TemporaryMarker allSpots <$> asks (view (#objects . #locations))
  playerMarkerLocs <- findResourceWithin (PlayerMarker pl) allSpots <$> asks (view (#objects . #locations))
  let tempMarkerPairs =  fmap locToNameHeight tempMarkerLocs
  let playerMarkerPairs =  fmap locToNameHeight playerMarkerLocs
  return (concatMap (resolveMarker' pl playerMarkerPairs) tempMarkerPairs)
  where
    resolveMarker' :: Player -> [(TrackName, TrackHeight)] -> (TrackName, TrackHeight) -> [CantStopGameNode]
    resolveMarker' player playerMarkers (nm, newHeight) = case lookup nm playerMarkers of
      Just h ->
        [ mkMoveNode player (MkTransfer (TrackSpot nm h) (TrackSpot nm newHeight) (PlayerMarker player)),
          mkMoveNode player (MkTransfer (TrackSpot nm newHeight) BoxTop TemporaryMarker)
        ]
      Nothing ->
        [ mkMoveNode player (MkTransfer (PlayerStuff pl) (TrackSpot nm newHeight) (PlayerMarker pl)),
          mkMoveNode player (MkTransfer (TrackSpot nm newHeight) BoxTop TemporaryMarker)
        ]

    locToNameHeight (TrackSpot nm h) = (nm, h)
    locToNameHeight _ = error "applied to non-track"

runCSNodes = runNodesAgainstState (initGameData 3) gameRules


--- Helpers ---
