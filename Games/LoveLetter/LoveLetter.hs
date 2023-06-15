{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# HLINT ignore "Use head" #-}
module LoveLetter
    where

import Objects
import Game.Helpers
import Game.Player
import Game.Location
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Game.Options (Options(..), Legality (..))
import Data.Maybe (mapMaybe, fromMaybe, listToMaybe)
import Data.List (delete)
import Game.GameNode (mkOptionsNode, GameAction (..), mkActionNode)
import Control.Monad (filterM, forM, join, (<=<))
import Util (maximaByScore)
import Data.List.NonEmpty (NonEmpty)
import Game.Monad (injectGame)
import Game.GameState (Phase(..), GameRules (..))

drawCard ::  Player -> LLM [LLGameNode]
drawCard p = nodeMaybe (mkTransfer TheDeck (Hand p)) <$> peek  TheDeck

playCard :: Player -> Character -> LLM [LLGameNode]
playCard p char = nodeMaybe (mkTransfer (Hand p) BoxTop) <$> hasMaybe (Hand p) (Card char)

swapHands :: Player -> Player -> LLM [LLGameNode]
swapHands p p' = unsafeSwapAll (Hand p) (Hand p')

discardCards :: Player -> LLM [LLGameNode]
discardCards p = do
    cards <- S.toList <$> whatsAt (Hand p)
    return (mkTransfer (Hand p) BoxTop <$> cards)

takeToken :: Player -> LLM [LLGameNode]
takeToken p = justTransfer BoxTop (Tokens p) Token

getCards p =  mapMaybe extractChar . S.toList <$> whatsAt (Hand p)
getCard = fmap listToMaybe . getCards

handFight :: Player -> Player -> LLM [LLGameNode]
handFight p p' = let
    pStrength = maximum . fmap charStrength <$> getCard p
    pStrength' = maximum . fmap charStrength <$> getCard p'
                  in do
                     battle <- compare <$> pStrength <*> pStrength'
                     case battle of 
                         GT -> discardCards p'
                         LT -> discardCards p
                         _ -> head doNothing -- TODO: lol


giveHandmaid :: Player -> LLM [LLGameNode]
giveHandmaid p = justTransfer BoxTop (HandmaidInd p) HandmaidMarker


chooseMove :: Player -> LLM [LLGameNode]
chooseMove p = do
    cards <- getCards p
    players <- lookPlayers 
    let otherPlayers = delete p (S.toList players) 
    targets <- targetablePlayers
    return [mkOptionsNode (buildPlay p otherPlayers targets (cards !! 0) (cards !! 1))]
           

checkGameOver :: LLM [LLGameNode]
checkGameOver = do
    keepGoing <- anyHas TheDeck startingCards
    if keepGoing
    then return [mkActionNode DoNothing]
    else do
        ps <- lookPlayers
        winners <- maximaByScore playerScore (S.toList ps)
        return [mkActionNode (EndGame winners)]
    where
        -- TODO: simplify
        playerScore :: Player -> LLM Int
        playerScore p = maybe 0 charStrength . (extractChar =<<) <$> peek (Hand p)


activePlayer :: (Player -> LLM [a]) -> LLM [a]
activePlayer action = maybe (pure []) action . currentPlayer =<< lookCurrentPhase

targetablePlayers :: LLM [Player]
targetablePlayers = let
    targetable p = has (HandmaidInd p) HandmaidMarker
    in
        filterM targetable =<< (S.toList <$> lookPlayers)

doNothing = [pure $ mkActionNode <$> [DoNothing]]

llRunPlay' :: LLPlayName -> [LLM [LLGameNode]]
llRunPlay' PlayPrincess = [activePlayer discardCards]
llRunPlay' PlayCountess = doNothing
llRunPlay' (PlayKing (Just p')) = [activePlayer (swapHands p')]
llRunPlay' (PlayKing Nothing) = doNothing
llRunPlay' (PlayPrince p') = [discardCards p', drawCard p']
llRunPlay' PlayHandmaid = [activePlayer giveHandmaid]
llRunPlay' (PlayBaron (Just p')) = [activePlayer (\p -> Hand p `revealTo` p')
                                  , activePlayer (Hand p' `revealTo`)
                                  , activePlayer (`handFight` p')
                                  , activePlayer (\p -> Hand p `unrevealTo` p')
                                  , activePlayer (Hand p' `unrevealTo`)
                                   ]
llRunPlay' (PlayBaron Nothing) = doNothing
llRunPlay' (PlayPriest (Just p')) = [activePlayer (Hand p' `revealTo`),
                                     activePlayer (Hand p' `unrevealTo`)]
llRunPlay' (PlayPriest Nothing) = doNothing
llRunPlay' (PlayGuard (Just (p',char))) = [do
    card <- getCard p'
    if card == Just char
    then discardCards p'
    else head doNothing -- TODO: lol again
                                          ]
llRunPlay' (PlayGuard Nothing ) = doNothing

llRunPlay = fmap injectGame . llRunPlay'

--

llPhases :: LLPhaseName -> Phase LLPhaseName LLLocation LLCounters LLResource LLPlayName LLIssue
llPhases (LLTurn p) = Phase (LLTurn p) (injectGame <$> [drawCard p])
llPhases Setup = undefined


llGameRules = GameRules llRunPlay llPhases 

