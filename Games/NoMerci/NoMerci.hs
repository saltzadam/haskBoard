{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}

module NoMerci where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as M
import Effectful (Eff)
import Game.Options (Legality (..), Options (..))
import Game.Player
import GameNode
import Objects
import Helpers (has, hasAny, transferAll, howManyAt)
import Location (peek, inventory)
import GameE (useGameState, location, currentPhase, Phase (..), GameState (..), TurnControl, actionTurns)
import Control.Lens (to)
import qualified Data.Set as S
import Data.Set (Set)
import Visibility (allVisible, VisibilityMap)
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Util (getNextCyclic, maximaByScore)

-- Plays --

shuffleDeck :: GameAction NMLocation NMCounters NMResource NMPhaseName
shuffleDeck = Shuffle CardDeck

drawCard :: Observe es => Eff es [NMGameNode]
drawCard = (:[]) <$> do
    possibleCard <- useGameState (location CardDeck . to peek)
    case possibleCard of
      Just card -> return $ mkActionNode (MkTransfer CardDeck CenterOfTableCard card)
      Nothing -> return $ mkActionNode DoNothing

takeCard :: Observe es => Player -> Eff es [NMGameNode]
takeCard p = do
    maybeCard <- useGameState (location CenterOfTableCard . to peek)
    case maybeCard of
      Just theCard -> fmap (mkActionNode (MkTransfer CenterOfTableCard (PlayerStuff p) theCard) :)
                                (transferAll ChipPile (PlayerStuff p) Chip)
      Nothing -> return [mkActionNode DoNothing]

payChip :: Observe es => Eff es [NMGameNode]
payChip = do
    phase <- useGameState #currentPhase
    let p = currentPlayer phase
    return [mkActionNode $ MkTransfer (PlayerStuff p) ChipPile Chip]


chooseMove :: Observe es => Player -> Eff es [NMGameNode]
chooseMove p = do
    pHasChips <- has (PlayerStuff p) Chip
    let options = if pHasChips
                  then Options (Take :| [Decline]) M.empty
                  else Options (NE.singleton Take) (M.singleton Decline (Illegal [NoMoreChips]))
    return [mkGetOptionsNode p options]

checkGameOver :: Observe es => Eff es [NMGameNode]
checkGameOver = do
    keepGoing <- hasAny CardDeck isCard
    if keepGoing
    then return [mkActionNode DoNothing]
    else do
        winners <- getWinners
        return [mkActionNode (EndGame winners)]

scoreCards :: Set Int -> Int
scoreCards cardValues = scoreSorted (S.toDescList cardValues) 0
    where
        scoreSorted (x:y:zs) currentScore = if x - y == 1
                                            then scoreSorted (y:zs) currentScore
                                            else scoreSorted (y:zs) (currentScore + x)
        scoreSorted [y] currentScore = currentScore + y
        scoreSorted [] currentScore = currentScore

score :: Observe es => Player -> Eff es Int
score p = do
    stuff <- useGameState (location (PlayerStuff p))
    let cardVals = S.fromList . fmap (\(Card i) -> i) . filter isCard $ M.keys . inventory $ stuff
    chips <- howManyAt (PlayerStuff p) Chip
    return (scoreCards cardVals - fromEnum chips)

getWinners :: Observe es => Eff es [Player]
getWinners = do
    players <- useGameState #players
    maximaByScore score (S.toList players)


checkEnd :: Observe es => Eff es [NMGameNode]
checkEnd = do
    cardsLeft <- hasAny CardDeck isCard
    if cardsLeft
    then return []
    else do 
        winners <- getWinners
        return [mkActionNode (EndGame winners)]

nmRunPlay :: Observe es => NMPlayName -> [Eff es [NMGameNode]]
nmRunPlay Take = [do
    phase <- useGameState #currentPhase
    let p = currentPlayer phase
    takeCard p,
                 checkEnd,
                 drawCard]
nmRunPlay Decline = [payChip]

-- -- Initialization --
nmPhases :: NMPhaseName -> NMPhase
nmPhases (NMTurn p) = Phase
    { name = NMTurn p,
      seedNodes = [chooseMove p]
    }

nmVisibility :: VisibilityMap l c
nmVisibility = allVisible

initGameState :: Int -> NMGameState
initGameState numPlayers =
  let players = Set.fromList (fmap Player [1 .. fromIntegral numPlayers])
   in GameState
        { players = players,
          objects = initGameObjects players,
          currentPhase = NMTurn (head (S.toList players)),
          phases = nmPhases,
          turns = fmap playerTurn (NE.fromList . S.toList $ players),
          currentTurn = playerTurn (Player 1),
          nextTurn = \t ts -> fromJust (getNextCyclic t ts) -- TODO: how to make this safe?
        }

-- Game states --

runNMTurns :: IO TurnControl
runNMTurns = actionTurns (initGameState 3) nmRunPlay allVisible drawCard


