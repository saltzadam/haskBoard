{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module CantStop where

import Control.Lens ((^.))
import Control.Monad (filterM, replicateM_, void, join)
import Data.Foldable (traverse_, forM_)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set.NonEmpty as NES
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, listToMaybe)
import qualified Data.Set as S
import Game.GameAction
import Game.GameState
import Game.Location (histogram)
import Game.Options (exceptIf, unlessYouCould, youMay, youMay')
import Game.Player
import Game.Rules
import Game.Visibility (allVisible)
import Helpers hiding (getNextTurn)
import Objects
import qualified Track
import Util
import Game.Options (exceptIf')
import Game.Options (Options(..))
import Game.Options (youMayOnly)

-- === Actions ===
rollDice :: CSM ()
rollDice = traverse_ roll theDice

moveMarkerBy :: Player -> CantStopResource -> TrackName -> Int -> CSM ()
moveMarkerBy pl marker s amt = do
  tempPos <- Track.rMaxHeight (PlayerMarker pl) (track s)
  let tempPos' = fromMaybe 0 tempPos
  replicateM_ amt (Track.advanceOrInsertAt marker BoxTop (track s) tempPos')

resolveMarkers :: Player -> CSM ()
resolveMarkers p = traverse_ (resolveTrack p) trackNames -- for each track...
  where
    resolveTrack :: Player -> TrackName -> CSM ()
    resolveTrack p trackName = do
      tempHeight <- Track.rMaxHeight TemporaryMarker (track trackName) -- get the height of the temp marker
      case tempHeight of 
        Nothing -> return () -- if no temp marker, nothing to resolve
        Just i -> do
          playerHeight <- Track.rMaxHeight (PlayerMarker p) (track trackName) -- get the height of `p`'s marker
          let diff = i - fromMaybe 0 playerHeight
          Track.transferFrom (track trackName) BoxTop TemporaryMarker -- remove the temp marker
          replicateM_ diff (Track.advanceOrInsert (PlayerMarker p) BoxTop (track trackName)) -- move p to it's position
          -- alternatively

clearMarkers :: Maybe Player -> TrackName -> CSM ()
clearMarkers mp trackName = Track.removeAll (marker mp) (track trackName) BoxTop

clearWonTracks :: CSM ()
clearWonTracks = do
  players <- lookPlayers
  -- traverse_ (clearTrackIfWon players) trackNames
  forM_ trackNames $ \trackName -> do
    maybeWinner <- trackWinner trackName
    case maybeWinner of
      Nothing -> doNothing
      Just p -> forM_ players $ \p' -> 
        if p' == p
        then doNothing
        else clearMarkers (Just p') trackName

checkWinner :: CSM ()
checkWinner = do
  trackWinners <- catMaybes <$> traverse trackWinner trackNames
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
enumeratePlays :: CSM (NES.NESet CantStopPlayName)
enumeratePlays = do
  diceValues <- traverse lookCounterVal theDice
  let pairs = fmap (uncurry (+)) <$> mkPairs diceValues
  let diceSingleValues = NE.fromList . concatMap (fmap diceToTrack) $ pairs
  -- TODO: still lame lol
  let dicePairVals = NE.fromList . fmap ((\[x, y] -> (x, y)) . fmap diceToTrack) $ pairs
  return $
    NES.fromList $
      (uncurry TwoMove  <$> dicePairVals) <> (OneMove <$> diceSingleValues)

getOptions :: Player -> CSM CantStopOptions
getOptions p = do
  -- allPossiblePlays <- youMay p enumeratePlays
  -- filt1' <- exceptIf' notEnoughMarkers allPossiblePlays
  -- case filt1' of
  --   Nothing -> youMayOnly p ForceStop
  --   Just filt1'' -> do
  --     filt2 <- (exceptIf onWonTrack (pure filt1''))
  --     case filt2 of
  --       Nothing -> youMayOnly p ForceStop
  --       Just filt2'' -> return filt2''
  -- --
  -- -- let x = maybe
  -- -- let filt1 =  exceptIf notEnoughMarkers
  -- -- let filt2 = exceptIf onWonTrack
  -- -- let comp x = fmap (join) $ (traverse filt2) (sequence $ filt1 x)
  -- --
  -- --
  -- -- return allPossiblePlays
  youMay p enumeratePlays 
    & exceptIf notEnoughMarkers ForceStop
    & exceptIf onWonTrack  ForceStop
    & exceptIf atTop  ForceStop
    & unlessYouCould (\pl0 pl1 -> return (thereIsBiggerMove pl0 pl1)) ForceStop
  where
    notEnoughMarkers :: CantStopPlayName -> CSM Bool
    notEnoughMarkers (TwoMove track0 track1) = do
      onBoard0 <- Track.count TemporaryMarker (track track0)
      onBoard1 <- Track.count TemporaryMarker (track track1)
      let needed = (if track0 == track1 then 1 else 2) - (onBoard0 + onBoard1)
      inBox <- howManyAt BoxTop TemporaryMarker
      return (needed > inBox)
    notEnoughMarkers (OneMove track0) = do
      onBoard <- Track.count TemporaryMarker (track track0)
      let needed = 1 - onBoard
      inBox <- howManyAt BoxTop TemporaryMarker
      return (needed > inBox)
    notEnoughMarkers _ = return False

    onWonTrack :: CantStopPlayName -> CSM Bool
    onWonTrack (OneMove s) = do
      maybeWinner <- trackWinner s
      return $ isJust maybeWinner
    onWonTrack (TwoMove s t) = (&&) <$> onWonTrack (OneMove s) <*> onWonTrack (OneMove t)
    onWonTrack _ = return False

    atTop :: CantStopPlayName -> CSM Bool
    atTop (OneMove s) = Track.resAtTop (PlayerMarker p) (track s)
    atTop (TwoMove s t) = (&&) <$> atTop (OneMove s) <*> atTop (OneMove t)
    atTop _ = return False

chooseMove :: Player -> CSM ()
chooseMove p = do
  opts <- getOptions p
  void $ makeChoice opts

stopOrGoNode :: Player -> CSM ()
stopOrGoNode p = void $ makeChoice (youMay' p (NES.fromList . NE.fromList $ [Stop, DontStop]))

trackWinner :: TrackName -> CSM (Maybe Player)
trackWinner trackName = do
  players <- lookPlayers
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
          nextTurn = playerTurn (Player 2),
          visibility = allVisible
        }

csRunPlay' :: CantStopPlayName -> CSM ()
csRunPlay' (TwoMove s t) = do
  activePlayer $ \pl -> moveMarkerBy pl TemporaryMarker s 1
  activePlayer $ \pl -> moveMarkerBy pl TemporaryMarker t 1
  activePlayer $ \pl -> stopOrGoNode pl
csRunPlay' (OneMove s) = do
  activePlayer $ \pl -> moveMarkerBy pl TemporaryMarker s 1
  activePlayer $ \pl -> stopOrGoNode pl
csRunPlay' (Stop) = do
  activePlayer $ \pl -> resolveMarkers pl
  clearWonTracks
  checkWinner
csRunPlay' (DontStop) = rollDice >> (activePlayer $ \pl -> chooseMove pl)
csRunPlay' (ForceStop) =
  traverse_ (clearMarkers Nothing) trackNames

score :: Player -> CSM Int
score p = length . filter (== Just p) <$> traverse trackWinner trackNames

-- cantStop :: CantStopGameRules
-- cantStop = GameRules csRunPlay' cantStopPhases score Nothing
