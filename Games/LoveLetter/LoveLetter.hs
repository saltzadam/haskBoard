{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}
{-# HLINT ignore "Use head" #-}
module LoveLetter where

import qualified Cards as Card
import Control.Applicative
import Control.Lens (view)
import Control.Monad (filterM, forM, join, void, (<=<))
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Game.GameState (GameRules (..), GameState (..), Phase (..), location)
import Game.Location (inventory)
import Game.Options
import Game.Player
import Game.Rules
import Game.Visibility (allVisible)
import Helpers
import Objects
import Text
import Util (ifM, maximaByScore, maximaByScoreM)

drawCard :: Player -> LLM ()
drawCard p = Card.draw TheDeck (Hand p)

playCard :: Player -> Character -> LLM ()
playCard p char = do
  announceGame (displayPlayerT p <> T.pack " played " <> icon char)
  Card.play (Hand p) BoxTop (Card char)

swapHands :: Player -> Player -> LLM ()
swapHands p p' = do
  announceGame (displayPlayerT p <> T.pack " and " <> displayPlayerT p' <> T.pack "swapped hands")
  unsafeSwapAll (Hand p) (Hand p')

discardCards :: Player -> LLM ()
discardCards p = do
  announceGame (displayPlayerT p <> T.pack " discarded their hand.")
  cardsInHand <- S.toList <$> whatsAt (Hand p)
  traverse_ (transfer (Hand p) BoxTop) cardsInHand

takeToken :: Player -> LLM ()
takeToken p = transfer BoxTop (Tokens p) Token

getCards p = mapMaybe extractChar . S.toList <$> whatsAt (Hand p)

getCard = fmap listToMaybe . getCards

handFight :: Player -> Player -> LLM ()
handFight p p' =
  let pStrength = maximum . fmap charStrength <$> getCard p
      pStrength' = maximum . fmap charStrength <$> getCard p'
   in do
        announceGame (displayPlayerT p <> T.pack " and " <> displayPlayerT p' <> T.pack " compare their hands")
        battle <- compare <$> pStrength <*> pStrength'
        case battle of
          GT -> discardCards p'
          LT -> discardCards p
          _ -> justDoNothing

giveHandmaid :: Player -> LLM ()
giveHandmaid p = transfer BoxTop (HandmaidInd p) HandmaidMarker

isAlive :: Player -> LLM Bool
isAlive p = Hand p `hasAny` cards

score :: Player -> GameRule LLLocation LLCounters LLResource LLPhaseName LLPlayName LLIssue Int
score p = ifM (isAlive p) 1 0

cardPlays :: Player -> Character -> LLM (NonEmpty LLPlayName)
cardPlays _ Princess = pure (NE.singleton PlayPrincess)
cardPlays _ Countess = pure (NE.singleton PlayCountess)
cardPlays p King = fmap PlayKing <$> targetablePlayers p
cardPlays p Prince = fmap PlayPrince <$> targetablePlayers p
cardPlays _ Handmaid = pure (NE.singleton PlayHandmaid)
cardPlays p Baron = fmap PlayBaron <$> targetablePlayers p
cardPlays p Priest = fmap PlayPriest <$> targetablePlayers p
cardPlays p Guard = do
  targets <- targetablePlayers p
  return (PlayGuard <$> targets <*> characters)

chooseMove :: Player -> LLM LLPlayName
chooseMove p = do
  charsHeld <- NE.fromList <$> getCards p -- TODO: is this solvable?
  let plays = join <$> traverse (cardPlays p) charsHeld -- less confusing if join = concat
  choices <- youMay p plays & unlessYouCould countessRule
  makeChoice choices
  where
    countessRule PlayCountess PlayPrincess = return $ oneIssue MustDiscardCountess
    countessRule _ _ = return Legal

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

targetablePlayers :: Player -> LLM (NonEmpty Player)
targetablePlayers p =
  let targetable p' = has (HandmaidInd p') HandmaidMarker -- TODO: must be alive
   in do
        otherPlayers <- S.toList <$> lookOtherPlayers p
        otherTargets <- filterM targetable otherPlayers
        return (p :| otherTargets)

untargetablePlayers :: Player -> LLM [Player]
untargetablePlayers p = do
  otherPlayersSet <- lookOtherPlayers p
  targets <- S.fromList . NE.toList <$> targetablePlayers p
  return (S.toList (otherPlayersSet `S.difference` targets))

llRunPlay' :: LLPlayName -> LLM ()
llRunPlay' PlayPrincess = activePlayer discardCards
llRunPlay' PlayCountess = justDoNothing
llRunPlay' (PlayKing p') = activePlayer (swapHands p')
llRunPlay' (PlayPrince p') = discardCards p' >> drawCard p'
llRunPlay' PlayHandmaid = activePlayer giveHandmaid
llRunPlay' (PlayBaron p') = do
  activePlayer (\p -> Hand p `revealTo` p')
  activePlayer (Hand p' `revealTo`)
  activePlayer (`handFight` p')
  activePlayer (\p -> Hand p `unrevealTo` p')
  activePlayer (Hand p' `unrevealTo`)
llRunPlay' (PlayPriest p') = do
  activePlayer (\p -> announceGame (displayPlayerT p <> T.pack " reveals their hand to " <> displayPlayerT p'))
  activePlayer (Hand p' `revealTo`)
  activePlayer (Hand p' `unrevealTo`)
llRunPlay' (PlayGuard p' char) = do
  card <- getCard p'
  if card == Just char
    then discardCards p'
    else justDoNothing

llRunPlay :: LLPlayName -> LLM ()
llRunPlay play =
  let theCard = Card (playToCharacter play)
   in do
        activePlayer (\p -> transfer (Hand p) PlayedCard theCard)
        llRunPlay' play
        transfer PlayedCard BoxTop theCard

--

llPhases :: LLPhaseName -> Phase LLPhaseName LLLocation LLCounters LLResource LLPlayName LLIssue
llPhases (LLTurn p) = Phase (LLTurn p) (drawCard p >> void (chooseMove p))
llPhases Setup =
  Phase
    Setup
    ( do
        shuffle TheDeck
        traverse_ drawCard =<< lookPlayers
    )

initGameState :: Int -> LLGameState
initGameState numPlayers =
  let players = S.fromList (mkPlayers numPlayers)
      -- alivePlayers :: LLGameState -> NonEmpty Player
      alivePlayer p gs = (sum . inventory $ view (location (Hand p)) gs) > 0
   in GameState
        { players = players,
          objects = initGameObjects players,
          currentPhase = LLTurn (head (S.toList players)),
          currentTurn = playerTurn (Player 1),
          nextTurn = getNextTurnIf playerTurn alivePlayer, -- TODO: how to make this safe?
          visibility = allVisible -- TODO: no, BoxTop is invisible
        }

llGameRules = GameRules llRunPlay llPhases score (Just Setup)

loveLetter i = (initGameState i, llGameRules)
