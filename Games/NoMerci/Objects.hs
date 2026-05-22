{-# LANGUAGE DeriveAnyClass #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Objects 
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Finitary
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import qualified Data.Set as S
import FinitaryMap (FTMap (..))
import NumberedPiece (NumberedPiece (..))
import GHC.Generics (Generic)
import Game.Agent
import Game.GameState (GameRules, GameState, Phase)
import Game.Location
import Game.Options (Options)
import Game.Player
import Game.Rules
import Game.View (GameStateView)

-- don't derive Functor so it's not easy to modify card nums
data NMResource = Chip | Card (NumberedPiece 35)
  deriving (Eq, Ord, Show, Generic, Finitary, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

extractCard :: NMResource -> Maybe Int
extractCard (Card (NumberedPiece i)) = Just (fromEnum i)
extractCard _ = Nothing

isCard :: NMResource -> Bool
isCard = isJust . extractCard

cards :: [NMResource]
cards = filter isCard inhabitants

scoreCards :: Set NMResource -> Int
scoreCards = scoreCards' . S.map fromJust . S.filter isJust . S.map extractCard

-- could be fun to rewrite as a fold with accumulator like (sum, prev_element)
-- scoreCards' :: Set Int -> Int
-- scoreCards' cardValues = scoreSorted (S.toAscList cardValues) 0
--   where
--     scoreSorted (x : y : zs) currentScore =
--       if y - x == 1
--         then scoreSorted (y : zs) currentScore
--         else scoreSorted (y : zs) (currentScore + x)
--     scoreSorted [y] currentScore = currentScore + y
--     scoreSorted [] currentScore = currentScore

-- It was fun!
scoreCards' :: Set Int -> Int
scoreCards' cardValues = fst $ foldr go (0, Nothing) (S.toDescList cardValues) -- toDescList because foldr works from the right side
  where
    go :: Int -> (Int, Maybe Int) -> (Int, Maybe Int)
    go i (agg, Nothing)  = (agg + i, Just i)
    go i (agg, Just prev)  = (agg + if i - prev == 1 then 0 else i, Just i)

data NMLocation
  = CenterOfTableCard
  | ChipPile
  | PlayerStuff Player
  | CardDeck
  | BoxTop
  deriving (Eq, Ord, Show, Generic, Finitary, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

type NMGameObjects = GameObjects NMLocation NoCounters NMResource

initLocations' :: Set Player -> NMLocation -> LocationShape NMResource
initLocations' _ CenterOfTableCard = emptySlot
initLocations' _ ChipPile = emptyPile
initLocations' _ CardDeck = deckOf cards 
initLocations' players (PlayerStuff player)
  | player `S.member` players = pileOf Chip 11
  | otherwise =  dummy
initLocations' _ BoxTop = emptyPile

-- todo: ergonomic
initLocations :: Set Player -> FTMap NMLocation (LocationShape NMResource)
initLocations ps = FTMap (initLocations' ps)

initGameObjects :: Set Player -> NMGameObjects
initGameObjects ps =
  GameObjects
    { locations = initLocations ps,
      -- todo: ergonomic
      counters = FTMap (const dummyCounter)
    }

data NMPlayName = Take | Decline deriving (Eq, Ord, Show, Generic, Finitary, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

data NMPhaseName = NMTurnPhase Player deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

type NMTurn = Turn NMPhaseName

type NMPhase = Phase NMPhaseName NMLocation NoCounters NMResource NMPlayName

type NMGameState = GameState NMLocation NoCounters NMResource NMPhaseName NMPlayName

type NMOptions = Options NMPlayName

type NMGameRules = GameRules NMLocation NoCounters NMResource NMPhaseName NMPlayName

type NMM a = GameRule NMLocation NoCounters NMResource NMPhaseName NMPlayName a

type NMView = GameStateView NMLocation NoCounters NMResource NMPhaseName

type NMEvent = BEvent NMLocation NoCounters NMResource NMPhaseName NMPlayName


-- todo: ergoonomic
playerTurn :: Player -> NMTurn
playerTurn p = Turn p (NE.singleton (NMTurnPhase p))
