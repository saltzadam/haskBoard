{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# LANGUAGE TypeOperators #-}

module NoMerci where

import Objects
import Game.GameNode (GameAction(..), mkActionNode, mkOptionsNode)
import Game.Location (peek)
import Game.Helpers (lookLocation, mkTransfer, nodeMaybe, transferAll, lookCurrentPhase, has, anyHas, lookPlayers, howManyAt, whatsAt)
import Game.Player (Player(..))
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import Game.Options (Options(..), Legality (..))
import qualified Data.Set as S
import Util (maximaByScore, getNextCyclic)
import Game.GameState (Phase(..), GameState (..), GameInteract, GameRules (..))
import Data.Maybe (fromJust)
import Game.Visibility (allVisible)
import Game.Monad (injectGame, runGameEff)
import Effectful (Eff, (:>))
import Debug.Trace 

-- Plays --


drawCard ::  NMM [NMGameNode]
drawCard = nodeMaybe (mkTransfer CardDeck CenterOfTableCard) . peek <$> lookLocation CardDeck

discardCard :: NMM [NMGameNode]
discardCard = nodeMaybe (mkTransfer CardDeck BoxTop) . peek <$> lookLocation CardDeck

takeCard ::  Player -> NMM [NMGameNode]
takeCard p = nodeMaybe (mkTransfer CenterOfTableCard (PlayerStuff p)) . peek <$> lookLocation CenterOfTableCard

-- TODO: list is lame
payChip :: NMM [NMGameNode]
payChip = nodeMaybe (\p -> mkTransfer (PlayerStuff p) ChipPile Chip) . currentPlayer <$> lookCurrentPhase

chooseMove :: Player -> NMM [NMGameNode]
chooseMove p = do
    pHasChips <- has (PlayerStuff p) Chip
    let options = if pHasChips
                  then Options (Take NE.:| [Decline]) M.empty p
                  else Options (NE.singleton Take) (M.singleton Decline (Illegal [NoMoreChips])) p
    return [mkOptionsNode options]

checkGameOver :: NMM [NMGameNode]
checkGameOver = do
    keepGoing <- anyHas CardDeck cards
    if keepGoing
    then return [mkActionNode DoNothing]
    else do
        winners <- getWinners
        return [mkActionNode (EndGame winners)]

score' :: Player -> NMM Int
score' p =  let
    cardScore = scoreCards <$> whatsAt (PlayerStuff p)
    chipScore = fromEnum <$> howManyAt (PlayerStuff p) Chip
            in cardScore - chipScore


getWinners :: NMM [Player]
getWinners =  maximaByScore score' . S.toList =<< lookPlayers

checkEnd :: NMM [NMGameNode]
checkEnd = do
    cardsLeft <- anyHas CardDeck cards
    if cardsLeft
    then return [mkActionNode DoNothing]
    else (:[]) . mkActionNode . EndGame <$> getWinners

nmRunPlay' ::  NMPlayName -> [NMM [NMGameNode]]
nmRunPlay' Take = [ maybe (pure []) takeCard . currentPlayer =<< lookCurrentPhase,
                  checkEnd,
                  drawCard,
                  maybe (pure []) chooseMove . currentPlayer =<< lookCurrentPhase]
nmRunPlay' Decline = [payChip,
                      pure [mkActionNode AdvanceTurn]]

nmRunPlay :: (GameInteract NMLocation NMCounters NMResource NMPhaseName NMPlayName NMIssue :> es0) => NMPlayName -> [Eff es0 [NMGameNode]]
nmRunPlay = fmap injectGame . nmRunPlay'

-- -- Initialization --
nmPhases :: NMPhaseName -> NMPhase
nmPhases (NMTurn p) = Phase
    { name = NMTurn p,
      seedNodes = injectGame <$> [chooseMove p]
    }
    -- TODO: ugly
nmPhases Setup = Phase {name = Setup,
    seedNodes = injectGame <$> (pure [mkActionNode (Shuffle CardDeck)] : (replicate 9 discardCard ++ [drawCard]))}



initGameState :: Int -> NMGameState
initGameState numPlayers =
  let players = S.fromList (fmap (Player . fromIntegral) [1 .. numPlayers])
   in GameState
        { players = players,
          objects = initGameObjects players,
          currentPhase = NMTurn (head (S.toList players)),
          turns = fmap playerTurn (NE.fromList . S.toList $ players),
          currentTurn = playerTurn (Player 1),
          nextTurn = \t ts -> fromJust (getNextCyclic t ts), -- TODO: how to make this safe?
          visibility = allVisible -- TODO: no, BoxTop is invisible
        }


score :: GameState
  NMLocation NMCounters NMResource NMPhaseName NMPlayName NMIssue -> Player -> Int
score gs= runGameEff gs . score'

noMerci :: Int -> (NMGameState, NMGameRules)
noMerci numPlayers = (initGameState numPlayers, GameRules nmRunPlay nmPhases score (Just Setup))

