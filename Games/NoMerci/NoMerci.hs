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
   in cardScore - chipScore

checkEnd :: NMM ()
checkEnd =
  ifM
    (CardDeck `hasAny` cards)
    justDoNothing
    ( do
        winners <- maximaByScoreM score . S.toList =<< lookPlayers
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

noMerci :: Int -> (NMGameState, NMGameRules)
noMerci numPlayers = (initGameState numPlayers, GameRules nmRunPlay nmPhases score (Just nmSetup))

