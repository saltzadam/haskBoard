{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module NoMerci (noMerci) where

import qualified Cards
import Control.Monad (replicateM_)
import qualified Data.Set as S
import Game.GameState (GameRules (..), GameState (..))
import Game.Options (Options (..))
import Game.Player (Player (..), mkPlayers)
import Game.Rules
import Game.Visibility (VisData (..), VisibilityMap (..), hideManyFromAll)
import Helpers
import Game.Location (NoCounters)
import Objects 
import Util (ifM, maximaByScoreM)
import Data.Maybe (listToMaybe, mapMaybe)
import NumberedPiece
import Control.Applicative ((<|>))
import qualified Data.Map as M


-- Plays --

drawCard :: NMM ()
drawCard = Cards.draw CardDeck CenterOfTableCard

chooseMove :: Player -> NMM ()
chooseMove p = do
  pHasChips <- PlayerStuff p `has` Chip
  let options =
        if pHasChips 
          then Options [Take, Decline] p
          else Options [Take] p
  makeChoice_ options

score :: Player -> NMM Int
score p =
  let cardScore = scoreCards <$> whatsAt (PlayerStuff p)
      chipScore = howManyAt (PlayerStuff p) Chip
   in -(cardScore - chipScore)

checkEnd :: NMM ()
checkEnd =
  ifM
    (CardDeck `hasAny` cards)
    justDoNothing
    ( do
        winners <- maximaByScoreM score =<< lookPlayers
        endGame winners
    )

nmRunPlay :: NMPlayName -> NMM ()
nmRunPlay Take = do
  -- todo: ergonomics
  activePlayer (Cards.draw CenterOfTableCard . PlayerStuff)
  activePlayer (transferAll ChipPile . PlayerStuff)
  checkEnd
  drawCard
  activePlayer chooseMove
nmRunPlay Decline = activePlayer (\p -> transfer (PlayerStuff p) ChipPile Chip)

-- Initialization --

visibility :: Int -> VisibilityMap NMLocation NoCounters
visibility numPlayers = hideManyFromAll (mkPlayers numPlayers) [VisLocation BoxTop, VisLocation CardDeck]

initGameState :: Int -> NMGameState
initGameState numPlayers =
  let players = S.fromList (mkPlayers numPlayers)
   in GameState
        { players = players,
          objects = initGameObjects players,
          currentPhase = NMTurnPhase (Player 1),
          currentTurn = playerTurn (Player 1),
          nextTurn    = playerTurn (Player 2),   -- will be overwritten by advanceTurnCyclic
          visibility = visibility numPlayers 
        }


nmSetup :: NMM ()
nmSetup = do 
  shuffle CardDeck
  replicateM_ 9 (Cards.draw CardDeck BoxTop) 
  drawCard

nmPhases :: NMPhaseName -> NMPhase
nmPhases name@(NMTurnPhase p) = mkPhase name (chooseMove p >> advanceTurnCyclic playerTurn)

-- The game

noMerci :: Int -> (NMGameState, NMGameRules, [NMHint])
noMerci numPlayers =
  ( initGameState numPlayers
  , GameRules
      { playRunner  = nmRunPlay
      , phases      = nmPhases
      , score       = score
      , scoreBounds = (-50, 200)
      , scorePublic = True
      , setupPhase  = Just nmSetup
      }
  , [takeRun, takeOverValued, takeForceOthers, takeGeneral]
  )


-- Hints
-- Take something if it makes a run
-- Take something if it has more chips than its value
-- if you have the most chips and the card is >=18, don't take it

hint x = return (Just x)
noHint = return (Nothing)

lookCenterCardVal :: GameRule NMLocation cn NMResource ph pl (Maybe Int)
lookCenterCardVal = do
  cardAvailable <- listToMaybe <$> listResAtF CenterOfTableCard isCard
  return $ cardAvailable >>= extractCard

-- TODO: use guard?
takeRun :: NMHint
takeRun pls = 
  do
    cardAvailable <- peek CenterOfTableCard
    currPlayer <- lookCurrentTurnOwner
    playerCards <- listResAtF (PlayerStuff currPlayer) isCard
    let playerVals = mapMaybe extractCard playerCards
    case cardAvailable >>= extractCard of
      Nothing -> noHint
      Just cardVal -> do
        if (cardVal + 1 `elem` playerVals) || (cardVal - 1 `elem` playerVals)
        then hint Take
        else noHint
    
takeOverValued :: NMHint
takeOverValued pls = 
    do
    cardAvailable <- peek CenterOfTableCard
    case cardAvailable >>= extractCard of
      Nothing -> noHint
      Just cardVal -> do
        chipsAvailable <- howManyAt ChipPile Chip
        if chipsAvailable >= cardVal then hint Take else noHint

takeForceOthers :: NMHint
takeForceOthers pls = do
  currPlayer <- lookCurrentTurnOwner 
  players <- lookPlayers
  playerChips <- sequence . M.fromList $  [(p, howManyAt (PlayerStuff p) Chip ) | p <- players]
  cardAvailable <- peek CenterOfTableCard
  case cardAvailable >>= extractCard of
    Nothing -> noHint
    Just cardVal -> 
      if and [playerChips M.! currPlayer > playerChips M.! p | p <- players, p /= currPlayer] 
          && cardVal >= 18 then hint Decline
          else noHint
  
takeGeneral :: NMHint
takeGeneral pls = do
  currPlayer <- lookCurrentTurnOwner
  players <- lookPlayers
  myChips <- howManyAt (PlayerStuff currPlayer) Chip
  cardAvailable <- peek CenterOfTableCard
  let cardAvailableValue = cardAvailable >>= extractCard
  chipsAvailable <- howManyAt ChipPile Chip

  maxPlayerChips <- fmap maximum . sequence $ [ howManyAt (PlayerStuff p) Chip | p <- players, p /= currPlayer]
  let cardAvailableValue  = cardAvailable >>= extractCard
  if myChips > 3 && Just (chipsAvailable + 3) >= cardAvailableValue -- TODO: not great
  then hint Take
  else if myChips > 3
       then hint Decline
       else do
         if chipsAvailable > maxPlayerChips - myChips then hint Take
           else noHint
          
    

  -- let playerCards' = fmap extractCard playerCards
  --
  -- let nextOrPrev card = nextMaybe card <|> prevMaybe card 
  -- case listToMaybe cardAvailable of
  --   Just (Card i) -> case nextMaybe i of 
  --     Just next -> ifM (CenterOfTableCard `has` Card next) (return $ const $ Just Take) (return $ const Nothing)
  --     Nothing -> case prevMaybe i of
  --       Just prev -> ifM (CenterOfTableCard `has` Card prev) (return $ const $ Just Take) (return $ const Nothing)
  --       Nothing -> return $ const Nothing
  --   _ -> return $ const Nothing
  --
  --
