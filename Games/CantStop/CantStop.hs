{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use =<<" #-}

module CantStop where

import Control.Lens (both, each, over, traverseOf, (^.), preview)
import Data.Finitary (inhabitants)
import Data.List (find, nub)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isJust, listToMaybe, mapMaybe)
import qualified Data.Set as S
import FinitaryMap (ftAt)
import Game.GameNode
import Game.GameState
import Game.Helpers
import Game.Location (histogram, inventory)
import Game.Monad (injectGame)
import Game.Options (Legality (..), Options (..),  raiseIssueIf, oneIssue, youMay, exceptIf, unlessYouCould)
import Game.Player
import Game.Visibility (allVisible)
import Objects
import Util
import Data.Function ((&))

-- Plays --

rollNodes :: CSM [CantStopGameNode]
rollNodes = pure $ action . RollCounter <$> [DieOne .. DieFour]

-- Three steps:
-- 1) enumerate possible moves (what p rolled)
--      Player -> CSM [PlayName]
-- 2) filter out single moves (illegal moves, e.g. you didn't roll 3 so you can't move 3)
--      CSM [PlayName] -> CSM (Map PlayName Legality)
-- 3) resolve sets of moves (you can move on both 6 and 7 so you can't move on them separately)
--      CSM (Map PlayName Legality -> Map PlayName Legality)
enumeratePlays :: Player -> CSM (NonEmpty CantStopPlayName)
enumeratePlays p =  NE.nub <$> 
                   (fmap (uncurry (TwoMove p)) <$> dicePairVals)
                <> (fmap (OneMove p) <$> diceSingleVals)
    where
    diceIntTuple = traverseOf each lookCounterVal theDiceL
    diceSingleVals = NE.fromList . fmap diceToTrack . concatMap (\(x,y) -> [x,y]) . mkPairs <$> diceIntTuple
    dicePairVals :: CSM (NonEmpty (TrackName, TrackName))
    dicePairVals = NE.fromList . fmap (over both diceToTrack) . mkPairs <$> diceIntTuple
    mkPairs :: (Eq a, Num a) => (a, a, a, a) -> [(a, a)]
    mkPairs (x, y, w, z) = nub [(x + y, w + z), (x + w, y + z), (x + z, y + w)]


getOptions :: Player -> CSM CantStopOptions
getOptions p = youMay p (enumeratePlays p) 
            & exceptIf notEnoughMarkers (pure (ForceStop p)) 
            & exceptIf onWonTrack (pure (ForceStop p))
            & exceptIf atTop (pure (ForceStop p))
            & unlessYouCould (\pl0 pl1 -> return (thereIsBiggerMove pl0 pl1))

chooseMove :: Player -> CSM [CantStopGameNode]
chooseMove p = (:[]) . choice <$> getOptions p

notEnoughMarkers :: CantStopPlayName -> CSM (Legality CantStopIssue)
notEnoughMarkers (TwoMove _ track0 track1) = do
    onBoard0 <- howManyWithin (trackSlots track0) TemporaryMarker
    onBoard1 <- howManyWithin (trackSlots track1) TemporaryMarker
    let needed = (if track0 == track1 then 1 else 2) - (onBoard0 + onBoard1)
    inBox <- howManyAt BoxTop TemporaryMarker
    return (if needed > inBox then oneIssue NotEnoughMarkers else Legal)
notEnoughMarkers (OneMove _ track) = do 
    onBoard <- howManyWithin (trackSlots track) TemporaryMarker
    let needed = 1 - onBoard
    inBox <- howManyAt BoxTop TemporaryMarker
    return (if needed > inBox then oneIssue NotEnoughMarkers else Legal)
notEnoughMarkers _ = return Legal

onWonTrack :: CantStopPlayName -> CSM (Legality CantStopIssue)
onWonTrack (OneMove _ s) = do
    maybeWinner <- trackWinner s
    case maybeWinner of
      Just _ -> return (oneIssue TrackCompleted)
      Nothing -> return Legal
onWonTrack (TwoMove p s t) = onWonTrack (OneMove p s) <> onWonTrack (OneMove p t)
onWonTrack _ = return Legal

atTop :: CantStopPlayName -> CSM (Legality CantStopIssue)
atTop (OneMove p s) = raiseIssueIf AtTop <$> maxSpot s `has` PlayerMarker p
atTop (TwoMove p s t) = atTop (OneMove p s) <> atTop (OneMove p t)
atTop _ = return Legal

stopOrGoNode :: Player -> CSM [CantStopGameNode]
stopOrGoNode p = (: []) . choice <$> stopOrGo p
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
       in [action (MkTransfer slot nextSlot TemporaryMarker)]
    -- if you can't find the temporary marker, look for the player marker on that track
    -- if you find it, place the TemporaryMarker
    (Nothing, Just (TrackSpot s height)) ->
      let gap = min (getHeight (maxSlot s) - getHeight height) amt
          nextSlot = TrackSpot s (height <+ gap)
       in [action (MkTransfer BoxTop nextSlot TemporaryMarker)]
    -- if you don't find it, just place the TemporaryMarker
    (Nothing, Nothing) -> [action (MkTransfer BoxTop (TrackSpot s (toEnum (amt - 1))) TemporaryMarker)]
    (_, _) -> [] -- This is an interesting case -- as long as Locations are all one type there will have to be lame patterns like this
    -- guess you can't stop someone from writing bad rules!

returnMarker :: CantStopLocation -> CantStopResource -> CantStopGameNode
returnMarker n (PlayerMarker p') = action (MkTransfer n (PlayerStuff p') (PlayerMarker p'))
returnMarker n TemporaryMarker = action (MkTransfer n BoxTop TemporaryMarker)

resolveMarkers :: Player -> CSM [CantStopGameNode]
resolveMarkers p = do
  tempMarkerLocs <- findResourceWithin' TemporaryMarker allSpots
  playerMarkerLocs <- findResourceWithin' (PlayerMarker p) allSpots
  return $ concatMap (resolveMarker' p playerMarkerLocs) tempMarkerLocs
  where
    resolveMarker' :: Player -> [CantStopLocation] -> CantStopLocation -> [CantStopGameNode]
    resolveMarker' player playerMarkers tempLoc =
      let currentPosition =
            fromMaybe (PlayerStuff player)
              . find (sameTrack tempLoc)
              $ playerMarkers
       in [ action (MkTransfer currentPosition tempLoc (PlayerMarker player)),
                  returnMarker tempLoc TemporaryMarker
                ]

clearTempMarkers :: Player -> CSM [CantStopGameNode]
clearTempMarkers _ =
  fmap (`returnMarker` TemporaryMarker)
    <$> findResourceWithin' TemporaryMarker allSpots

clearWonTracks :: CSM [CantStopGameNode]
clearWonTracks = do
  trackWinnerList <- M.toList <$> trackWinners
  foldMap (uncurry moveOtherMarkers) trackWinnerList
  where
    moveOtherMarkers :: TrackName -> Player -> CSM [CantStopGameNode]
    moveOtherMarkers track winningP = do
      let mkReturns :: CantStopLocation -> CSM [CantStopGameNode]
          mkReturns track = fmap (returnMarker track) <$> listResAtF track (/= PlayerMarker winningP)
      foldMap mkReturns (trackSlots track)

advancePlayer :: CSM [CantStopGameNode]
advancePlayer = return [action AdvanceTurn]

checkWinner :: CSM [CantStopGameNode]
checkWinner = do
  trackMap <- trackWinners
  let playerHistogram = histogram trackMap
  let winners = M.keys . M.filter (>= 3) $ playerHistogram
  if not (null winners)
    then return [action (EndGame winners)]
    else return [action DoNothing]

trackWinner :: TrackName -> CSM (Maybe Player)
trackWinner track = do
  things <- lookLocation (maxSpot track)
  return $ listToMaybe . mapMaybe markerOwner . M.keys . M.filter (> 0) $ inventory things

trackWinners :: CSM (Map TrackName Player)
trackWinners = graphMapM trackWinner inhabitants

-- TODO: bummer that this is defined from GS...
-- TODO: also bummer this uses labels
score :: CantStopGameState -> Player -> Int
score gs p =
  let topSpots = (\l -> gs ^. #objects . #locations . ftAt l) . maxSpot <$> inhabitants
      pscore = (M.! PlayerMarker p) . inventory <$> topSpots
   in sum pscore

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
csRunPlay' (ForceStop p) =
  [ clearTempMarkers p,
    advancePlayer
  ]

csRunPlay = fmap injectGame . csRunPlay'

cantStop :: CantStopGameRules
cantStop = GameRules csRunPlay cantStopPhases score Nothing
