{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use =<<" #-}

module CantStop where

import Control.Lens (both, each, over, preview, traverseOf, (^.))
import Control.Monad (filterM, forM, join, replicateM)
import Data.Bitraversable
import Data.Function ((&))
import Data.List (find, nub)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, listToMaybe, mapMaybe)
import qualified Data.Set as S
import Game.GameNode
import Game.GameState
import Game.Helpers
import Game.Location (histogram, inventory)
import Game.Monad (injectGame, runGameEff)
import Game.Options (Legality (..), Options (..), exceptIf, oneIssue, raiseIssueIf, unlessYouCould, youMay)
import Game.Player
import Game.Visibility (allVisible)
import Objects
import Util

-- Plays --

rollNodes :: CSM [CantStopGameNode]
rollNodes = pure $ action . RollCounter <$> [Die DieOne, Die DieTwo, Die DieThree, Die DieFour]

-- Three steps:
-- 1) enumerate possible moves (what p rolled)
--      Player -> CSM [PlayName]
-- 2) filter out single moves (illegal moves, e.g. you didn't roll 3 so you can't move 3)
--      CSM [PlayName] -> CSM (Map PlayName Legality)
-- 3) resolve sets of moves (you can move on both 6 and 7 so you can't move on them separately)
--      CSM (Map PlayName Legality -> Map PlayName Legality)
enumeratePlays :: Player -> CSM (NonEmpty CantStopPlayName)
enumeratePlays p =
  NE.nub
    <$> (fmap (uncurry (TwoMove p)) <$> dicePairVals)
      <> (fmap (OneMove p) <$> diceSingleVals)
  where
    -- TODO: possible to use some type-level stuff to distinguish nullable counters?
    diceIntTuple = traverseOf each (fmap fromJust . lookCounterVal) theDiceL
    diceSingleVals = NE.fromList . fmap diceToTrack . concatMap (\(x, y) -> [x, y]) . mkPairs <$> diceIntTuple
    dicePairVals :: CSM (NonEmpty (TrackName, TrackName))
    dicePairVals = NE.fromList . fmap (over both diceToTrack) . mkPairs <$> diceIntTuple
    mkPairs :: (Eq a, Num a) => (a, a, a, a) -> [(a, a)]
    mkPairs (x, y, w, z) = nub [(x + y, w + z), (x + w, y + z), (x + z, y + w)]

getOptions :: Player -> CSM CantStopOptions
getOptions p =
  youMay p (enumeratePlays p)
    & exceptIf notEnoughMarkers (pure (ForceStop p))
    & exceptIf onWonTrack (pure (ForceStop p))
    & exceptIf atTop (pure (ForceStop p))
    & unlessYouCould (\pl0 pl1 -> return (thereIsBiggerMove pl0 pl1))

chooseMove :: Player -> CSM [CantStopGameNode]
chooseMove p = (: []) . choice <$> getOptions p

notEnoughMarkers :: CantStopPlayName -> CSM (Legality CantStopIssue)
notEnoughMarkers (TwoMove _ track0 track1) = do
  onBoard0 <- fmap boolToInt (counterHasVal (TempTrack track0))
  onBoard1 <- fmap boolToInt (counterHasVal (TempTrack track1))
  let needed = (if track0 == track1 then 1 else 2) - (onBoard0 + onBoard1)
  inBox <- fmap length . filterM (counterHasVal . TempTrack) $ [Two .. Twelve]
  return (if needed > inBox then oneIssue NotEnoughMarkers else Legal)
notEnoughMarkers (OneMove _ track) = do
  onBoard <- fmap boolToInt (counterHasVal (TempTrack track))
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
atTop (OneMove p s) = raiseIssueIf AtTop <$> counterAtMax (PlayerTrack p s)
atTop (TwoMove p s t) = atTop (OneMove p s) <> atTop (OneMove p t)
atTop _ = return Legal

stopOrGoNode :: Player -> CSM [CantStopGameNode]
stopOrGoNode p = (: []) . choice <$> stopOrGo p
  where
    stopOrGo :: Player -> CSM CantStopOptions
    stopOrGo p = return (Options (NE.fromList [Stop p, DontStop p]) mempty p)

moveMarkerBy :: Player -> TrackName -> Int -> CSM [CantStopGameNode]
moveMarkerBy p s amt = mconcat $ replicate amt (advanceTrack (PlayerTrack p s))

returnMarker :: Maybe Player -> TrackName -> CSM [CantStopGameNode]
returnMarker (Just p) track = removeFromTrack (PlayerTrack p track)
returnMarker Nothing track = removeFromTrack (TempTrack track)

returnPlayerMarker :: Player -> TrackName -> CSM [CantStopGameNode]
returnPlayerMarker p = returnMarker (Just p)

resolveMarkers :: Player -> CSM [CantStopGameNode]
resolveMarkers p = do
  tempMarkerLocs <- mapKeysCatMaybes getTrack <$> valueMap [TempTrack track | track <- [Two .. Twelve]]
  playerMarkerLocs <- mapKeysCatMaybes getTrack <$> valueMap [PlayerTrack p track | track <- [Two .. Twelve]]
  mconcat $ fmap resolveMarker' $ M.toList $ M.intersectionWith subtract tempMarkerLocs playerMarkerLocs
  where
    resolveMarker' :: (TrackName, Int) -> CSM [CantStopGameNode]
    resolveMarker' (trackName, amount) = returnMarker Nothing trackName <> advanceTrackTimes (PlayerTrack p trackName) amount

-- TODO: duplicate! define Maybe Player -> TrackName -> (Int -> Counter)
clearMarkers :: Maybe Player -> CSM [CantStopGameNode]
clearMarkers Nothing = do
  tracks <- M.keys . mapKeysCatMaybes getTrack <$> valueMap [TempTrack track | track <- [Two .. Twelve]]
  moves <- traverse (returnMarker Nothing) tracks
  return (concat moves)
clearMarkers (Just p) = do
  tracks <- M.keys . mapKeysCatMaybes getTrack <$> valueMap [PlayerTrack p track | track <- [Two .. Twelve]]
  moves <- traverse (returnMarker Nothing) tracks
  return (concat moves)

advancePlayer :: CSM [CantStopGameNode]
advancePlayer = return [action AdvanceTurn]

trackWinner :: TrackName -> CSM (Maybe Player)
trackWinner track = do
  countersAtTop <- filterM counterAtMax allTracks
  let countersAtTop' = mapMaybe (\t -> bisequence (getTrack t, getTrackOwner t)) countersAtTop
  return (lookup track countersAtTop')

trackWinners :: CSM (Map TrackName Player)
trackWinners = graphMapM trackWinner [Two .. Twelve]

checkWinner :: CSM [CantStopGameNode]
checkWinner = do
  trackMap <- trackWinners
  let playerHistogram = histogram trackMap
  let winners = M.keys . M.filter (>= 3) $ playerHistogram
  if not (null winners)
    then return [action (EndGame winners)]
    else return [action DoNothing]

clearWonTracks :: CSM [CantStopGameNode]
clearWonTracks = do
  trackWinnerList <- M.toList <$> trackWinners
  let clearAllOtherMarkers (track, p) = mconcat . fmap (`returnPlayerMarker` track) . filter (/= p) $ allPlayers
  foldMap clearAllOtherMarkers trackWinnerList

score' :: Player -> CSM Int
score' p = length . filter (== p) . M.elems <$> trackWinners

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
  [ clearMarkers Nothing,
    advancePlayer
  ]

csRunPlay = fmap injectGame . csRunPlay'

cantStop :: CantStopGameRules
cantStop = GameRules csRunPlay cantStopPhases (\gs p -> runGameEff gs (score' p)) Nothing
