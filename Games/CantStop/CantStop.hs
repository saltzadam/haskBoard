{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module CantStop where

import Control.Lens (both, each, over, preview, sequenceOf, traverseOf, (^.))
import Control.Monad (filterM, forM, join, replicateM)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, listToMaybe)
import qualified Data.Set as S
import qualified Debug.Trace as Debug
import GHC.Base (Semigroup (stimes))
import Game.GameNode
import Game.GameState
import Game.Location (histogram, inventory)
import Game.Monad (injectGame', runGameEff)
import Game.Options (Legality (..), Options (..), exceptIf, oneIssue, raiseIssueIf, unlessYouCould, youMay)
import Game.Player
import Game.Visibility (allVisible)
import Helpers
import Objects
import qualified Track
import Util

-- Plays --

rollNodes :: CSM [CantStopGameNode]
rollNodes = pure $ concatMap roll theDice

-- Three steps:
-- 1) enumerate possible moves (what p rolled)
--      Player -> CSM [PlayName]
-- 2) filter out single moves (illegal moves, e.g. you didn't roll 3 so you can't move 3)
--      CSM [PlayName] -> CSM (Map PlayName Legality)
-- 3) resolve sets of moves (you can move on both 6 and 7 so you can't move on them separately)
--      CSM (Map PlayName Legality -> Map PlayName Legality)
enumeratePlays :: Player -> CSM (NonEmpty CantStopPlayName)
enumeratePlays p = do
  diceIntTuple <- traverse lookCounterVal theDice
  let pairs = fmap (uncurry (+)) <$> mkPairs diceIntTuple
  let diceSingleValues = NE.fromList . concatMap (fmap diceToTrack) $ pairs
  -- TODO: still lame lol
  let dicePairVals = NE.fromList . fmap ((\[x, y] -> (x, y)) . fmap diceToTrack) $ pairs
  return $
    NE.nub $
      (uncurry (TwoMove p) <$> dicePairVals) <> (OneMove p <$> diceSingleValues)

getOptions :: Player -> CSM CantStopOptions
getOptions p =
  youMay p (enumeratePlays p)
    & exceptIf notEnoughMarkers (pure (ForceStop p))
    & exceptIf onWonTrack (pure (ForceStop p))
    & exceptIf atTop (pure (ForceStop p))
    & unlessYouCould (\pl0 pl1 -> return (thereIsBiggerMove pl0 pl1))

chooseMove :: Player -> CSM [CantStopGameNode]
chooseMove p = fmap mkChoice (getOptions p)

notEnoughMarkers :: CantStopPlayName -> CSM (Legality CantStopIssue)
notEnoughMarkers (TwoMove _ track0 track1) = do
  onBoard0 <- Track.count TemporaryMarker (track track0)
  onBoard1 <- Track.count TemporaryMarker (track track1)
  let needed = (if track0 == track1 then 1 else 2) - (onBoard0 + onBoard1)
  inBox <- howManyAt BoxTop TemporaryMarker
  return (if needed > inBox then oneIssue NotEnoughMarkers else Legal)
notEnoughMarkers (OneMove _ track0) = do
  onBoard <- Track.count TemporaryMarker (track track0)
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
atTop (OneMove p s) = do
  maxSlot <- Track.resAtTop (PlayerMarker p) (track s)
  return (if maxSlot then oneIssue AtTop else Legal)
atTop (TwoMove p s t) = atTop (OneMove p s) <> atTop (OneMove p t)
atTop _ = return Legal

stopOrGoNode :: Player -> CSM [CantStopGameNode]
stopOrGoNode p = mkChoice <$> youMay p (pure $ NE.fromList [Stop p, DontStop p])

moveMarkerBy :: CantStopResource -> TrackName -> Int -> CSM [CantStopGameNode]
moveMarkerBy marker s amt = fmap concat . sequence $ (replicate amt (Track.advanceOrInsert marker BoxTop (track s)))

returnMarker :: Maybe Player -> TrackName -> CSM [CantStopGameNode]
returnMarker maybep trackName = Track.transferFrom (track trackName) BoxTop (maybe TemporaryMarker PlayerMarker maybep)

resolveMarkers :: Player -> [CSM [CantStopGameNode]]
resolveMarkers p = fmap (resolveTrack p) [Two .. Twelve]
  where
    resolveTrack :: Player -> TrackName -> CSM [CantStopGameNode]
    resolveTrack p trackName = do
      tempHeight <- Track.resMaxHeight TemporaryMarker (track trackName)
      playerHeight <- Track.resMaxHeight (PlayerMarker p) (track trackName)
      case tempHeight of
        Nothing -> return []
        Just i ->
          let diff = i - fromMaybe 0 playerHeight
           in returnMarker Nothing trackName <> mtimes diff (Track.advanceOrInsert (PlayerMarker p) BoxTop (track trackName))

clearMarkers :: Maybe Player -> TrackName -> CSM [CantStopGameNode]
clearMarkers mp trackName = Track.removeAll (marker mp) (track trackName) BoxTop

clearWonTracks :: CSM [CantStopGameNode]
clearWonTracks = do
  players <- S.toList <$> lookPlayers
  foldMap (clearTrackIfWon players) [Two .. Twelve]
  where
    clearTrackIfWon :: [Player] -> TrackName -> CSM [CantStopGameNode]
    clearTrackIfWon players trackName = do
      maybeWinner <- trackWinner trackName
      case maybeWinner of
        Just p -> mconcat [clearMarkers (Just p') trackName | p' <- players, p' /= p]
        Nothing -> pure []

advancePlayer :: CSM [CantStopGameNode]
advancePlayer = return [action AdvanceTurn]

checkWinner :: CSM [CantStopGameNode]
checkWinner = do
  trackWinners <- catMaybes <$> traverse trackWinner [Two .. Twelve]
  let winners = M.keys . M.filter (>= 3) $ histogram trackWinners
  if not (null winners)
    then return [action (EndGame winners)]
    else return [action DoNothing]

trackWinner :: TrackName -> CSM (Maybe Player)
trackWinner trackName = do
  players <- S.toList <$> lookPlayers
  winners <- filterM (\p -> Track.resAtTop (PlayerMarker p) (track trackName)) players
  return (listToMaybe winners) -- should only be one but not guaranteed by types

score :: CantStopGameState -> Player -> Int
score gs p = runGameEff gs (score' p)
  where
    score' p = length . filter (== Just p) <$> traverse trackWinner [Two .. Twelve]

-- Initialization --
cantStopPhases :: CantStopPhaseName -> CantStopPhase
cantStopPhases (CSTurn p) =
  Phase
    { name = CSTurn p,
      seedNodes = injectGame' <$> [rollNodes, chooseMove p]
    }

getNextTurn :: CantStopGameState -> CantStopTurn
getNextTurn gs =
  let Turn p _ = (gs ^. #currentTurn)
      players = NE.fromList (S.toList (gs ^. #players))
   in playerTurn (fromJust (getNextCyclic p players))

initGameState :: Int -> CantStopGameState
initGameState numPlayers =
  let players = S.fromList (fmap Player [1 .. fromIntegral numPlayers])
   in GameState
        { players = players,
          objects = initGameObjects players,
          currentPhase = CSTurn (head (S.toList players)),
          currentTurn = playerTurn (Player 1),
          nextTurn = getNextTurn,
          visibility = allVisible
        }

csRunPlay' :: CantStopPlayName -> [CSM [CantStopGameNode]]
csRunPlay' (TwoMove pl s t) =
  [ moveMarkerBy TemporaryMarker s 1,
    moveMarkerBy TemporaryMarker t 1,
    stopOrGoNode pl
  ]
csRunPlay' (OneMove pl s) =
  [ moveMarkerBy TemporaryMarker s 1,
    stopOrGoNode pl
  ]
csRunPlay' (Stop pl) =
  resolveMarkers pl
    ++ [ clearWonTracks,
         checkWinner,
         advancePlayer
       ]
csRunPlay' (DontStop p) = [rollNodes, chooseMove p]
csRunPlay' (ForceStop p) =
  fmap (clearMarkers Nothing) [Two .. Twelve]
    <> [advancePlayer]

csRunPlay = fmap injectGame' . csRunPlay'

cantStop :: CantStopGameRules
cantStop = GameRules csRunPlay cantStopPhases score Nothing
