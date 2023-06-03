{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use =<<" #-}

module CantStop where

import Count (Cnt, histogramF)
import Data.Finitary (inhabitants)
import Data.List (find, nub)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isJust, listToMaybe, mapMaybe)
import qualified Data.Set as S
import Game.GameNode
import Game.GameState
import Game.Helpers
import Game.Location (inventory)
import Game.Monad (injectGame)
import Game.Options (Legality (..), Options (..), buildOptions, mustNotElse, mustElse, raiseIssueIf)
import Game.Player
import Game.Visibility (allVisible)
import Objects
import Util (graphMapM, getNextCyclic, graph, splitOnFirst, buildSafeNonempty, andA)
import Control.Lens (each, traverseOf, (^.), over, both)
import FinitaryMap (ftAt)
import Data.Semigroup (sconcat)
import Data.List.NonEmpty (NonEmpty)

-- Plays --

rollNodes :: CSM [CantStopGameNode]
rollNodes = pure $ mkActionNode . RollCounter <$> inhabitants

chooseMove :: Player -> CSM [CantStopGameNode]
chooseMove p = (: []) . mkGetOptionsNode p <$> legalMoves p
  where
    legalMoves :: Player -> CSM CantStopOptions
    legalMoves p = do
        diceVals' <- diceVals
        moveSets <- traverse (checkMoveSet p) diceVals'
        -- Debug.traceShowM moveSets
        let (legal, illegal) = sconcat moveSets
        -- Debug.traceShowM legal
        -- Debug.traceShowM illegal
        if null legal then return $ Options (NE.singleton (ForceStop p)) illegal p
                      else return $ Options (NE.fromList (nub legal)) illegal p
    checkMove' :: Player -> TrackName -> CSM (Legality CantStopIssue)
    checkMove' _ track = do
      noMarkersLeft <- raiseIssueIf NotEnoughMarkers <$> (BoxTop `doesNotHave` TemporaryMarker) `andA` (null <$>  findResourceWithin' TemporaryMarker (trackSlots track))
      trackWon <- raiseIssueIf TrackCompleted . isJust <$> trackWinner track
      atTop <- raiseIssueIf AtTop <$> maxSpot track `has` PlayerMarker p
      return (noMarkersLeft <> trackWon <> atTop)
    checkMove :: CantStopPlayName -> CSM (Legality CantStopIssue)
    checkMove (TwoMove p s t) = checkMove' p s <> checkMove' p t <> do
        onBoard_s <- howManyWithin (trackSlots s) TemporaryMarker
        onBoard_t <- howManyWithin (trackSlots t) TemporaryMarker
        let needed = (if s == t then 1 else 2) - (onBoard_s + onBoard_t)

        inBox <- howManyAt BoxTop TemporaryMarker
        -- Debug.traceShowM (s, t, onBoard_s, onBoard_t, inBox, needed)
        return  (raiseIssueIf NotEnoughMarkers (needed > inBox))
    checkMove (OneMove p s) = checkMove' p s
    checkMove _ = pure Legal
    checkMoveSet :: Player -> (TrackName, TrackName) -> CSM ([CantStopPlayName],
                   Map CantStopPlayName (Legality CantStopIssue))
    checkMoveSet p (x, y) = do
        twoLegal <- checkMove (TwoMove p x y)
        if twoLegal == Legal
        then return ([TwoMove p x y], M.fromList [(OneMove p x, Illegal [CanMoveTwo]), (OneMove p y, Illegal [CanMoveTwo])])
        else do
            checkx <- checkMove (OneMove p x)
            checky <- checkMove (OneMove p y)
            let legalities = [(TwoMove p x y, twoLegal), (OneMove p x, checkx), (OneMove p y, checky)]
            let (legalMoves, illegalMoves) = M.partition (== Legal) . M.fromList $ legalities
            return (M.keys legalMoves, illegalMoves)

    diceVals :: CSM (NonEmpty (TrackName, TrackName))
    diceVals = NE.fromList . fmap (over both diceToTrack) . mkPairs <$> traverseOf each lookCounterVal theDiceL
    mkPairs :: (Eq a, Num a) => (a, a, a, a) -> [(a, a)]
    mkPairs (x, y, w, z) = nub [(x + y, w + z), (x+ w, y+ z), (x+ z, y+ w)]

stopOrGoNode :: Player -> CSM [CantStopGameNode]
stopOrGoNode p = (: []) . mkGetOptionsNode p <$> stopOrGo p
  where
    stopOrGo :: Player -> CSM CantStopOptions
    stopOrGo p = return (Options (NE.fromList [Stop p, DontStop p]) mempty p)

moveMarkerBy :: Player -> TrackName -> Int -> CSM [CantStopGameNode]
moveMarkerBy pl s amt = do
  let slots = trackSlots s
  tempMarkerLocs <- findResourceWithin' TemporaryMarker slots
  playerMarkerLocs <- findResourceWithin' (PlayerMarker pl) slots
  -- TODO: rewrite with Alternative
  return $ case (listToMaybe tempMarkerLocs, listToMaybe playerMarkerLocs) of
    -- if the TemporaryMarker is out, move 2 (or 1 if there's not enough space)
    (Just slot@(TrackSpot s height), _) ->
      let gap = min (getHeight (maxSlot s) - getHeight height) amt -- this is >0, otherwise move would be illegal
      -- NOT TRUE: easier to allow gap = 0 to be a noop.
      -- but will be >= 0.
          nextSlot = TrackSpot s (height <+ gap) -- target slot
       in [mkActionNode (MkTransfer slot nextSlot TemporaryMarker)]
    -- if you can't find the temporary marker, look for the player marker on that track
    -- if you find it, place the TemporaryMarker
    (Nothing, Just (TrackSpot s height)) ->
      let gap = min (getHeight (maxSlot s) - getHeight height) amt
          nextSlot = TrackSpot s (height <+ gap)
       in [mkActionNode (MkTransfer BoxTop nextSlot TemporaryMarker)]
    -- if you don't find it, just place the TemporaryMarker
    (Nothing, Nothing) -> [mkActionNode (MkTransfer BoxTop (TrackSpot s (toEnum (amt - 1))) TemporaryMarker)]
    (_, _) -> [] -- This is an interesting case -- as long as Locations are all one type there will have to be lame patterns like this
    -- guess you can't stop someone from writing bad rules!

returnMarker :: CantStopLocation -> CantStopResource -> GameAction CantStopLocation cn CantStopResource ph
returnMarker n (PlayerMarker p') = MkTransfer n (PlayerStuff p') (PlayerMarker p')
returnMarker n TemporaryMarker = MkTransfer n BoxTop TemporaryMarker

resolveMarkers :: Player -> CSM [CantStopGameNode]
resolveMarkers pl = do
  tempMarkerLocs <- findResourceWithin' TemporaryMarker allSpots
  playerMarkerLocs <- findResourceWithin' (PlayerMarker pl) allSpots
  return $ concatMap (resolveMarker' pl playerMarkerLocs) tempMarkerLocs
  where
    resolveMarker' :: Player -> [CantStopLocation] -> CantStopLocation -> [CantStopGameNode]
    resolveMarker' player playerMarkers newLoc@(TrackSpot track _) =
      let currentPosition =
            fromMaybe (PlayerStuff player)
              . find (\(TrackSpot track' _) -> track' == track)
              $ playerMarkers
       in  mkMoveNode player
            <$> [ MkTransfer currentPosition newLoc (PlayerMarker player),
                  returnMarker newLoc TemporaryMarker
                ]
    resolveMarker' _ _ _ = error "resolveMarker' called with newLoc not a TrackSpot!"

clearTempMarkers :: Player -> CSM [CantStopGameNode]
clearTempMarkers p =
  fmap (mkMoveNode p . (`returnMarker` TemporaryMarker))
    <$> findResourceWithin' TemporaryMarker allSpots

clearWonTracks :: CSM [CantStopGameNode]
clearWonTracks = do
  trackWinnerList <- M.toList <$> trackWinners
  concat <$> traverse (uncurry moveOtherMarkers) trackWinnerList
  where
    moveOtherMarkers :: TrackName -> Player -> CSM [CantStopGameNode]
    moveOtherMarkers track winningP = do
      let -- TODO: could still simplify
          getMarkersToMove :: CantStopLocation -> CSM [CantStopResource]
          getMarkersToMove n = listAllFM n (/= PlayerMarker winningP)
          mkReturns track = fmap (mkActionNode . returnMarker track) <$> getMarkersToMove track
      concat <$> traverse mkReturns (trackSlots track)

advancePlayer :: CSM [CantStopGameNode]
advancePlayer = return [mkActionNode AdvanceTurn]

checkWinner :: CSM [CantStopGameNode]
checkWinner = do
  trackMap <- trackWinners
  let playerHistogram = histogramF trackMap
  let winners = M.keys . M.filter (>= 3) $ playerHistogram
  if not (null winners)
    then return [mkActionNode (EndGame winners)]
    else return [mkActionNode DoNothing]

trackWinner :: TrackName -> CSM (Maybe Player)
trackWinner track = do
  things <- lookLocation (maxSpot track)
  return $ listToMaybe . mapMaybe markerOwner . M.keys . M.filter (> 0) $ inventory things

trackWinners :: CSM (Map TrackName Player)
trackWinners = graphMapM trackWinner inhabitants

-- TODO: bummer that this is defined from GS...
score :: CantStopGameState -> Player -> Cnt Int
score gs p = let
        topSpots = (\l -> gs ^. #objects . #locations . ftAt l) . maxSpot <$> inhabitants
        pscore = (M.! PlayerMarker p) . inventory <$> topSpots
    in
        sum pscore

-- Initialization --
cantStopPhases :: CantStopPhaseName -> CantStopPhase
cantStopPhases (CSTurn p) =
  Phase
    { name = CSTurn p,
      seedNodes = injectGame <$> [rollNodes, chooseMove p]
    }

initGameState :: Int -> CantStopGameState
initGameState numPlayers =
  let players = S.fromList (fmap Player [1 .. fromIntegral numPlayers])
   in GameState
        { players = players,
          objects = initGameObjects players,
          currentPhase = CSTurn (head (S.toList players)),
          turns = fmap playerTurn (NE.fromList . S.toList $ players),
          currentTurn = playerTurn (Player 1),
          nextTurn = \t ts -> fromJust (getNextCyclic t ts), -- TODO: how to make this safe?
          visibility = allVisible
        }

csRunPlay' :: CantStopPlayName -> [CSM [CantStopGameNode]]
csRunPlay' (TwoMove pl s t) =
  [ moveMarkerBy pl s 1,
    moveMarkerBy pl t 1,
    stopOrGoNode pl
  ]
csRunPlay' (OneMove pl s) =
    [ moveMarkerBy pl s 1,
      stopOrGoNode pl
    ]
csRunPlay' (Stop pl) =
  [ resolveMarkers pl,
    clearWonTracks,
    checkWinner,
    advancePlayer
  ]
csRunPlay' (DontStop p) = [rollNodes, chooseMove p]
csRunPlay' (ForceStop p) = [
    clearTempMarkers p,
    advancePlayer
  ]

csRunPlay = fmap injectGame . csRunPlay'

cantStop :: Int -> CantStopGame
cantStop numPlayers = Game (initGameState numPlayers) csRunPlay cantStopPhases (pure []) score
