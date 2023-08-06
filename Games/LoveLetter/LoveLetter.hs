{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}
{-# HLINT ignore "Use head" #-}
module LoveLetter where

import qualified Cards as Card
import Control.Monad (filterM, forM, join, (<=<))
import Data.Foldable (traverse_)
import Data.List (delete)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as S
import Game.GameState (GameRules (..), Phase (..))
import Game.Location ()
import Game.Player
import Game.Rules
import Helpers
import Objects
import Util (ifM, maximaByScore, maximaByScoreM)

drawCard :: Player -> LLM ()
drawCard p = Card.draw TheDeck (Hand p)

playCard :: Player -> Character -> LLM ()
playCard p char = Card.play (Hand p) BoxTop (Card char)

swapHands :: Player -> Player -> LLM ()
swapHands p p' = unsafeSwapAll (Hand p) (Hand p')

discardCards :: Player -> LLM ()
discardCards p = do
  cards <- S.toList <$> whatsAt (Hand p)
  traverse_ (transfer (Hand p) BoxTop) cards

takeToken :: Player -> LLM ()
takeToken p = transfer BoxTop (Tokens p) Token

getCards p = mapMaybe extractChar . S.toList <$> whatsAt (Hand p)

getCard = fmap listToMaybe . getCards

handFight :: Player -> Player -> LLM ()
handFight p p' =
  let pStrength = maximum . fmap charStrength <$> getCard p
      pStrength' = maximum . fmap charStrength <$> getCard p'
   in do
        battle <- compare <$> pStrength <*> pStrength'
        case battle of
          GT -> discardCards p'
          LT -> discardCards p
          _ -> justDoNothing

giveHandmaid :: Player -> LLM ()
giveHandmaid p = transfer BoxTop (HandmaidInd p) HandmaidMarker

isAlive :: Player -> LLM Bool
isAlive p = (Hand p) `hasAny` cards

-- chooseMove :: Player -> LLM ()
-- chooseMove p = do
--   cards <- getCards p
--   otherPlayers <- lookOtherPlayers
--   targets <- targetablePlayers
--   return [mkOptionsNode (buildPlay p otherPlayers targets (cards !! 0) (cards !! 1))]

checkGameOver :: LLM ()
checkGameOver =
  ifM
    (anyHas TheDeck startingCards)
    justDoNothing
    ( do
        ps <- lookPlayers
        winners <- maximaByScoreM playerScore (S.toList ps)
        endGame winners
    )
  where
    -- TODO: simplify
    playerScore :: Player -> LLM Int
    playerScore p = maybe 0 charStrength . (extractChar =<<) <$> peek (Hand p)

targetablePlayers :: LLM [Player]
targetablePlayers =
  let targetable p = has (HandmaidInd p) HandmaidMarker -- TODO: must be alive
   in filterM targetable =<< (S.toList <$> lookPlayers)

llRunPlay' :: LLPlayName -> LLM ()
llRunPlay' PlayPrincess = activePlayer discardCards
llRunPlay' PlayCountess = justDoNothing
llRunPlay' (PlayKing (Just p')) = activePlayer (swapHands p')
llRunPlay' (PlayKing Nothing) = justDoNothing
llRunPlay' (PlayPrince p') = discardCards p' >> drawCard p'
llRunPlay' PlayHandmaid = activePlayer giveHandmaid
llRunPlay' (PlayBaron (Just p')) = do
  activePlayer (\p -> Hand p `revealTo` p')
  activePlayer (Hand p' `revealTo`)
  activePlayer (`handFight` p')
  activePlayer (\p -> Hand p `unrevealTo` p')
  activePlayer (Hand p' `unrevealTo`)
llRunPlay' (PlayBaron Nothing) = justDoNothing
llRunPlay' (PlayPriest (Just p')) = do
  activePlayer (Hand p' `revealTo`)
  activePlayer (Hand p' `unrevealTo`)
llRunPlay' (PlayPriest Nothing) = justDoNothing
llRunPlay' (PlayGuard (Just (p', char))) = do
  card <- getCard p'
  if card == Just char
    then discardCards p'
    else justDoNothing
llRunPlay' (PlayGuard Nothing) = justDoNothing

llRunPlay :: LLPlayName -> LLM ()
llRunPlay play =
  let theCard = Card (playToCharacter play)
   in do
        activePlayer (\p -> transfer (Hand p) PlayedCard theCard)
        llRunPlay' play
        transfer PlayedCard BoxTop theCard

--

llPhases :: LLPhaseName -> Phase LLPhaseName LLLocation LLCounters LLResource LLPlayName LLIssue
llPhases (LLTurn p) = Phase (LLTurn p) [drawCard p]
llPhases Setup = Phase Setup [shuffle TheDeck, ]

llGameRules = GameRules llRunPlay llPhases
