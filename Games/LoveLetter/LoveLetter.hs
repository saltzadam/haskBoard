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
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (delete)
import Game.GameNode (mkOptionsNode, GameAction (..), mkActionNode)
import Control.Monad (filterM, forM, join, (<=<))
import Util (maximaByScore)

drawCard ::  Player -> LLM [LLGameNode]
drawCard p = nodeMaybe (mkTransfer TheDeck (Hand p)) . peek <$> lookLocation TheDeck

playCard :: Player -> Character -> LLM [LLGameNode]
playCard p char = nodeMaybe (mkTransfer (Hand p) PlayedCard) <$> hasMaybe (Hand p) (Card char)

discardCards :: Player -> LLM [LLGameNode]
discardCards p = do
    cards <- S.toList <$> whatsAt (Hand p)
    return (mkTransfer (Hand p) DiscardPile <$> cards)

takeToken :: Player -> LLM [LLGameNode]
takeToken p = return [mkTransfer TokenPile (Tokens p) Token]

chooseMove :: Player -> LLM [LLGameNode]
chooseMove p = do
    cards <- whatsAt (Hand p)
    let chars = mapMaybe extractChar (S.toList cards)
        options = if Card Countess `elem` cards
                     -- TODO: head
        then let otherChar = head . delete Countess $ chars
                 otherPlay char = M.singleton (Play char) (Illegal [MustDiscardCountess])
             in Options (NE.singleton (Play Countess)) (otherPlay otherChar) p
        else Options (NE.fromList (Play <$> chars)) M.empty p
    return [mkOptionsNode options]

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
        playerScore p = maybe 0 charStrength . (extractChar <=< peek) <$> lookLocation (Hand p)


activePlayer :: (Player -> LLM [a]) -> LLM [a]
activePlayer action = maybe (pure []) action . currentPlayer =<< lookCurrentPhase

targetablePlayers :: LLM [Player]
targetablePlayers = 

llRunPlay' :: LLPlayName -> [LLM [LLGameNode]]
llRunPlay' (Play Princess) = [activePlayer discardCards]
llRunPlay' (Play Countess) = [pure $ mkActionNode <$> [DoNothing]]
llRunPlay' (Play King) = 

