{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module CantStop where

import Control.Lens ((^.))
import Control.Monad (filterM, replicateM_, void)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, listToMaybe)
import qualified Data.Set as S
import qualified Debug.Trace as Debug
import Game.GameAction
import Game.GameState
import Game.Location (histogram)
import Game.Options (Legality (..), exceptIf, oneIssue, unlessYouCould, youMay, youMay')
import Game.Player
import Game.Rules
import Game.Visibility (allVisible)
import Helpers hiding (getNextTurn)
import Objects
import qualified Track
import Util

-- === Actions ===
rollDice :: CSM ()
rollDice = traverse_ roll theDice

moveMarkerBy :: Player -> CantStopResource -> TrackName -> Int -> CSM ()
moveMarkerBy pl marker s amt = do
  tempPos <- Track.resMaxHeight (PlayerMarker pl) (track s)
  let tempPos' = fromMaybe 0 tempPos
  replicateM_ amt (Track.advanceOrInsertAt marker BoxTop (track s) tempPos')

returnMarker :: Maybe Player -> TrackName -> CSM ()
returnMarker maybep trackName = Track.transferFrom (track trackName) BoxTop (maybe TemporaryMarker PlayerMarker maybep)

resolveMarkers :: Player -> CSM ()
resolveMarkers p = traverse_ (resolveTrack p) [Two .. Twelve]
  where
    resolveTrack :: Player -> TrackName -> CSM ()
    resolveTrack p trackName = do
      tempHeight <- Track.resMaxHeight TemporaryMarker (track trackName)
      playerHeight <- Track.resMaxHeight (PlayerMarker p) (track trackName)
      case tempHeight of
        Nothing -> return ()
        Just i ->
          let diff = i - fromMaybe 0 playerHeight
           in returnMarker Nothing trackName
                >> replicateM_ diff (Track.advanceOrInsert (PlayerMarker p) BoxTop (track trackName))

clearMarkers :: Maybe Player -> TrackName -> CSM ()
clearMarkers mp trackName = Track.removeAll (marker mp) (track trackName) BoxTop

clearWonTracks :: CSM ()
clearWonTracks = do
  players <- S.toList <$> lookPlayers
  traverse_ (clearTrackIfWon players) [Two .. Twelve]
  where
    clearTrackIfWon :: [Player] -> TrackName -> CSM ()
    clearTrackIfWon players trackName = do
      maybeWinner <- trackWinner trackName
      case maybeWinner of
        Just p -> sequence_ [clearMarkers (Just p') trackName | p' <- players, p' /= p]
        Nothing -> pure ()

checkWinner :: CSM ()
checkWinner = do
  trackWinners <- catMaybes <$> traverse trackWinner [Two .. Twelve]
  let winners = M.keys . M.filter (>= 3) $ histogram trackWinners
  if not (null winners)
    then act (EndGame winners)
    else act DoNothing

-- === Plays ===

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
  where
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
    onWonTrack (TwoMove p s t) = onWonTrack (OneMove p s) >> onWonTrack (OneMove p t)
    onWonTrack _ = return Legal

    atTop :: CantStopPlayName -> CSM (Legality CantStopIssue)
    atTop (OneMove p s) = do
      maxSlot <- Track.resAtTop (PlayerMarker p) (track s)
      return (if maxSlot then oneIssue AtTop else Legal)
    atTop (TwoMove p s t) = atTop (OneMove p s) >> atTop (OneMove p t)
    atTop _ = return Legal

chooseMove :: Player -> CSM ()
chooseMove p = do
  opts <- getOptions p
  void $ makeChoice opts

stopOrGoNode :: Player -> CSM ()
stopOrGoNode p = void $ makeChoice (youMay' p (NE.fromList [Stop p, DontStop p]))

trackWinner :: TrackName -> CSM (Maybe Player)
trackWinner trackName = do
  players <- S.toList <$> lookPlayers
  winners <- filterM (\p -> Track.resAtTop (PlayerMarker p) (track trackName)) players
  return (listToMaybe winners) -- should only be one but not guaranteed by types

-- Initialization --
cantStopPhases :: CantStopPhaseName -> CantStopPhase
cantStopPhases (CSTurn p) =
  Phase
    { name = CSTurn p,
      seedNodes = rollDice >> chooseMove p
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

csRunPlay' :: CantStopPlayName -> CSM ()
csRunPlay' (TwoMove pl s t) = do
  moveMarkerBy pl TemporaryMarker s 1
  moveMarkerBy pl TemporaryMarker t 1
  stopOrGoNode pl
csRunPlay' (OneMove pl s) = do
  moveMarkerBy pl TemporaryMarker s 1
  stopOrGoNode pl
csRunPlay' (Stop pl) = do
  resolveMarkers pl
  clearWonTracks
  checkWinner
  advanceTurn
csRunPlay' (DontStop p) = rollDice >> chooseMove p
csRunPlay' (ForceStop _) =
  traverse_ (clearMarkers Nothing) [Two .. Twelve]
    >> advanceTurn

score :: Player -> CSM Int
score p = length . filter (== Just p) <$> traverse trackWinner [Two .. Twelve]

cantStop :: CantStopGameRules
cantStop = GameRules csRunPlay' cantStopPhases score Nothing
