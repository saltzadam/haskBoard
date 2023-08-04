{-# HLINT ignore "Use list comprehension" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module NoMerci where

import qualified Cards
import Control.Monad (void)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S
import Game.GameAction (GameAction (..))
import Game.GameState (GameInteract, GameRules (..), GameState (..), Phase (..), Turn (..))
import Game.Options (Legality (..), Options (..), oneIssue)
import Game.Player (Player (..), mkPlayers)
import Game.Visibility (allVisible)
import Helpers
import Objects
import Util (ifM)

-- Plays --

discardCardFromDeck :: NMM ()
discardCardFromDeck = Cards.draw CardDeck BoxTop

takeCard :: Player -> NMM ()
takeCard p = Cards.draw CenterOfTableCard (PlayerStuff p)

takeChips :: Player -> NMM ()
takeChips p = transferAll ChipPile (PlayerStuff p) Chip

drawCard :: NMM ()
drawCard = Cards.draw CardDeck CenterOfTableCard

-- TODO: list is lame
payChip :: NMM ()
payChip = do
  p <- lookCurrentTurnOwner
  transfer (PlayerStuff p) ChipPile Chip

chooseMove :: Player -> NMM ()
chooseMove p = do
  pHasChips <- has (PlayerStuff p) Chip
  let options =
        if pHasChips -- TODO: improve
          then Options (Take NE.:| [Decline]) M.empty p
          else Options (NE.singleton Take) (M.singleton Decline (oneIssue NoMoreChips)) p
  void $ makeChoice options -- TODO: no void

score :: Player -> NMM Int
score p =
  let cardScore = scoreCards <$> whatsAt (PlayerStuff p)
      chipScore = fromEnum <$> howManyAt (PlayerStuff p) Chip
   in cardScore - chipScore

-- getWinners :: NMM [Player]
-- getWinners = maximaByScore score' . S.toList =<< lookPlayers
getWinners = undefined

checkEnd :: NMM ()
checkEnd =
  ifM
    (anyHas CardDeck cards)
    justDoNothing
    (getWinners >>= act . EndGame) -- TODO: why not just EndGame as a trigger to compute score?

activePlayer :: (Player -> NMM ()) -> NMM () -- TODO: use turnOwner, add to Helpers
activePlayer action = lookCurrentTurnOwner >>= action

nmRunPlay :: NMPlayName -> NMM ()
nmRunPlay Take = do
  activePlayer takeCard
  activePlayer takeChips
  checkEnd
  drawCard
  activePlayer chooseMove
nmRunPlay Decline = do
  payChip
  advanceTurn

-- -- Initialization --
nmPhases :: NMPhaseName -> NMPhase
nmPhases (NMTurn p) =
  Phase
    { name = NMTurn p,
      seedNodes = [chooseMove p]
    }
nmPhases Setup =
  Phase
    { name = Setup,
      seedNodes = shuffle CardDeck : (replicate 9 discardCardFromDeck ++ [drawCard])
    }

initGameState :: Int -> NMGameState
initGameState numPlayers =
  let players = S.fromList (mkPlayers numPlayers)
   in GameState
        { players = players,
          objects = initGameObjects players,
          currentPhase = NMTurn (head (S.toList players)),
          -- turns = fmap playerTurn (NE.fromList . S.toList $ players),
          currentTurn = playerTurn (Player 1),
          nextTurn = getNextTurn playerTurn, -- TODO: how to make this safe?
          visibility = allVisible -- TODO: no, BoxTop is invisible
        }

noMerci :: Int -> (NMGameState, NMGameRules)
noMerci numPlayers = (initGameState numPlayers, GameRules nmRunPlay nmPhases score (Just Setup))
