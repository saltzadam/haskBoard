{-# OPTIONS_GHC -Wno-name-shadowing #-}

module NoMerci (noMerci) where

import qualified Cards
import Control.Monad (replicateM_)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Game.GameState (GameRules (..), GameState (..), Phase (..))
import Game.Options (Options (..))
import Game.Player (Player (..), mkPlayers)
import Game.Rules
import Game.Visibility (allVisible)
import Helpers
import Objects
import Util (ifM, maximaByScoreM)

-- Plays --

drawCard :: NMM ()
drawCard = Cards.draw CardDeck CenterOfTableCard

chooseMove :: Player -> NMM ()
chooseMove p = do
  pHasChips <- has (PlayerStuff p) Chip
  let options =
        if pHasChips -- TODO: improve
          then Options (Take NE.:| [Decline]) p
          else Options (NE.singleton Take) p
  makeChoice_ options

score :: Player -> NMM Int
score p =
  let cardScore = scoreCards <$> whatsAt (PlayerStuff p)
      chipScore = fromEnum <$> howManyAt (PlayerStuff p) Chip
   in cardScore - chipScore

checkEnd :: NMM ()
checkEnd =
  ifM
    (anyHas CardDeck cards)
    justDoNothing
    ( do
        winners <- maximaByScoreM score . S.toList =<< lookPlayers
        endGame winners
    )

nmRunPlay :: NMPlayName -> NMM ()
nmRunPlay Take = do
  activePlayer (\p -> Cards.draw CenterOfTableCard (PlayerStuff p))
  activePlayer (\p -> transferAll ChipPile (PlayerStuff p) Chip)
  checkEnd
  drawCard
  activePlayer chooseMove
nmRunPlay Decline = activePlayer (\p -> transfer (PlayerStuff p) ChipPile Chip) >> advanceTurn

-- -- Initialization --
nmPhases :: NMPhaseName -> NMPhase
nmPhases (NMTurnPhase p) =
  Phase
    { name = NMTurnPhase p,
      seedNodes = setNextTurnCyclic playerTurn >> chooseMove p
    }
nmPhases Setup =
  Phase
    { name = Setup,
      seedNodes = shuffle CardDeck >> replicateM_ 9 (Cards.draw CardDeck BoxTop) >> drawCard
    }

initGameState :: Int -> NMGameState
initGameState numPlayers =
  let players = S.fromList (mkPlayers numPlayers)
   in GameState
        { players = players,
          objects = initGameObjects players,
          currentPhase = NMTurnPhase (head (S.toList players)),
          currentTurn = playerTurn (Player 1),
          -- nextTurn = Just $ getNextTurn2 playerTurn (NE.fromList . S.toList $ players), -- TODO: how to make this safe?
          nextTurn = Just $ playerTurn (Player 1),
          visibility = allVisible -- TODO: no, BoxTop is invisible
        }

noMerci :: Int -> (NMGameState, NMGameRules)
noMerci numPlayers = (initGameState numPlayers, GameRules nmRunPlay nmPhases score (Just Setup))
