{-# OPTIONS_GHC -Wno-name-shadowing #-}

module CantStop where

import Count (Cnt, histogramF)
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.List (find)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe, fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Set as Set
import Game.Options (Legality (..), Options (..), buildOptions, mustNotElse)
import Game.Player
import Game.GameState
import Game.GameNode
import Game.Location (inventory, listAllF)
import Objects
import Util (graphMapM, getNext)
import Game.Visibility (VisibilityMap (..), allVisible)
import Game.Helpers
import Data.Finitary (inhabitants)
-- import Game.Run (runEffNodesAgainstState, runNodesAgainstState, actionTurns)
import Effectful (runPureEff)
import qualified Effectful.State.Static.Shared as Eff
import qualified Effectful.Reader.Static as Eff
import Game.Monad (ViewerType(..), runGameEff)

-- Plays --

rollNodes :: CSM [CantStopGameNode]
rollNodes = pure $ mkActionNode . RollCounter <$> inhabitants

chooseMove :: Player -> CSM [CantStopGameNode]
chooseMove p = (: []) . mkGetOptionsNode p <$> legalMoves p
  where
    legalMoves :: Player -> CSM CantStopOptions
    legalMoves p = buildOptions p checkMove (Stop p) (fmap (makeMove p) <$> diceVals)
    makeMove :: Player -> (Cnt Int, Cnt Int) -> PlayName
    makeMove p (x, y) = Move p (diceToTrack x) (diceToTrack y)
    checkMove' :: Player -> TrackName -> CSM (Legality Issue)
    checkMove' p track = do
      noMarkersLeft <- flip mustNotElse ThreeTempMarkersOut <$> BoxTop `doesNotHave` TemporaryMarker
      trackWon <- flip mustNotElse TrackCompleted . isJust <$> trackWinner track
      atTop <- flip mustNotElse AtTop <$> maxSpot track `has` PlayerMarker p
      return (noMarkersLeft <> trackWon <> atTop)
    checkMove :: PlayName -> CSM (Legality Issue)
    checkMove (Move p s t) = do
      check_s <- checkMove' p s
      check_t <- checkMove' p t
      -- Legality is defined by (essentially) the First monoid. What is this alternative structure?
      if check_s == Legal || check_t == Legal
        then return Legal
        else return (check_s <> check_t)
    -- TODO: is this true?
    checkMove _ = pure Legal
    diceVals :: CSM [(Cnt Int, Cnt Int)]
    diceVals = mapM (bitraverse makeSum makeSum) (mkPairs theDiceL)
      where
        makeSum :: (CantStopCounterName, CantStopCounterName) -> CSM (Cnt Int)
        makeSum (c, c') = viewCounterVal c + viewCounterVal c'
        mkPairs :: (a, a, a, a) -> [((a, a), (a, a))]
        mkPairs (x, y, w, z) =
          let pairs1 = [((x, y), (w, z)), ((x, w), (y, z)), ((x, z), (y, w))]
           in -- pairs2 = fmap swap pairs1
              pairs1 -- ++ pairs2

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
      let gap =  min (getHeight (maxSlot s) - getHeight height) amt -- this is >0, otherwise move would be illegal
          -- NOT TRUE: easier to allow gap = 0 to be a noop.
          -- but will be >= 0.
          nextSlot = TrackSpot s (height <+ gap) -- target slot
       in [mkActionNode (MkTransfer slot nextSlot TemporaryMarker)]
    -- if you can't find the temporary marker, look for the player marker on that track
    -- if you find it, place the TemporaryMarker
    (Nothing, Just (TrackSpot s height)) ->
      let gap =  min (getHeight (maxSlot s) - getHeight height) amt
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
  return (concatMap (resolveMarker' pl playerMarkerLocs) tempMarkerLocs)
  where
    resolveMarker' :: Player -> [CantStopLocation] -> CantStopLocation -> [CantStopGameNode]
    resolveMarker' player playerMarkers newLoc@(TrackSpot track _) =
      let currentPosition =
            fromMaybe (PlayerStuff player)
              . find (\(TrackSpot track' _) -> track' == track)
              $ playerMarkers
       in mkMoveNode player
            <$> [ MkTransfer currentPosition newLoc (PlayerMarker player),
                  returnMarker newLoc TemporaryMarker
                ]
    resolveMarker' _ _ _ = error "resolveMarker' called with newLoc not a TrackSpot!"

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
          -- mkReturns :: CantStopLocation -> CSM [GameAction CantStopLocation cn CantStopResource ph]
          mkReturns track = fmap (mkActionNode . returnMarker track) <$> getMarkersToMove track
      -- mkActionNode 
      concat <$> traverse mkReturns (trackSlots track)

advancePlayer :: CSM [CantStopGameNode]
advancePlayer = do
  players <- viewPlayers
  currentPlayer <- fmap currentPlayer viewCurrentPhase
  let nextp = unsafeNextCyclic currentPlayer players
  return [mkActionNode (ChangePhase (CSTurn nextp))]
  where
    unsafeNextCyclic :: Player -> Set Player -> Player
    unsafeNextCyclic player players = go player (S.toList players) (S.toList players)
      where
        go p (p' : p'' : ps) allps = if p == p' then p'' else go p (p'' : ps) allps
        go _ [_] allps = head allps

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
  things <- viewLocation (maxSpot track)
  return $ listToMaybe . mapMaybe markerOwner . M.keys . M.filter (> 0) $ inventory things

trackWinners :: CSM (Map TrackName Player)
trackWinners = graphMapM trackWinner inhabitants

-- Initialization --
setupNodes :: Int -> [a]
setupNodes = pure []

csVisibility :: VisibilityMap CantStopLocation CantStopCounterName CantStopPhaseName
csVisibility = allVisible
cantStopPhases :: CantStopPhaseName -> CantStopPhase
cantStopPhases (CSTurn p) =
  Phase
    { name = CSTurn p,
    -- TODO: c'mon
      seedNodes = \gs -> runGameEff gs . fmap concat . sequence $ [rollNodes, chooseMove p]
    }

initGameState :: Int -> CantStopGameState
initGameState numPlayers =
  let players = Set.fromList (fmap Player [1 .. fromIntegral numPlayers])
   in GameState
        { players = players,
          objects = initGameObjects players,
          currentPhase = CSTurn (head (S.toList players)),
          phases = cantStopPhases,
          turns = fmap playerTurn (NE.fromList . S.toList $ players),
          currentTurn = playerTurn (Player 0),
          nextTurn = \t ts -> fromJust (getNext t ts), -- TODO: how to make this safe?
          visibility = csVisibility
        }


csRunPlay' :: PlayName -> CSM [CantStopGameNode]
csRunPlay' (Move pl s t) = concat <$> sequence [
    moveMarkerBy pl s 1
    , moveMarkerBy pl t 1
    , stopOrGoNode pl]
csRunPlay' (Stop pl) = concat <$> sequence [
    resolveMarkers pl
    , clearWonTracks
    , checkWinner
    , advancePlayer]
csRunPlay' (DontStop p) = rollNodes <> chooseMove p

csRunPlay :: CantStopGameState -> PlayName -> [CantStopGameNode]
csRunPlay gs pl = runGameEff gs (csRunPlay' pl)

-- Try with Turns instead

playerTurn :: Player -> CantStopTurn
playerTurn p = Turn p (NE.singleton (CSTurn p))

cantStop :: Int -> CantStopGame
cantStop numPlayers = Game (initGameState numPlayers) csRunPlay setupNodes

-- -- Game states --

-- runEffCSNodes effNodes = do
--   let gs = initGameState 3
--   runEffNodesAgainstState gs csRunPlay effNodes

-- runCSNodes :: [CantStopGameNode] -> IO CantStopGameState
-- runCSNodes = runNodesAgainstState (initGameState 3) csRunPlay setupNodes

-- moreInterestingGameState :: IO CantStopGameState
-- moreInterestingGameState =
--   runCSNodes
--     [ mkActionNode (MkTransfer (PlayerStuff (Player 2)) (TrackSpot Six HThree) (PlayerMarker (Player 2))),
--       mkActionNode (MkTransfer (PlayerStuff (Player 1)) (TrackSpot Two HOne) (PlayerMarker (Player 1))),
--       mkActionNode (MkTransfer (PlayerStuff (Player 3)) (TrackSpot Ten HTwo) (PlayerMarker (Player 3)))
--     ]


-- -- run game from Turns

-- runCSTurns :: IO CantStopGameState
-- runCSTurns = actionTurns (initGameState 3) csRunPlay setupNodes


