{-# LANGUAGE DeriveAnyClass #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Objects where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.Finitary
import Data.Finite (Finite)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import FinitaryMap (FTMap (..))
import GHC.Generics (Generic)
import Game.Agent
import Game.GameState (GameRules, GameState, Phase)
import Game.Location
import Game.Options (Options)
import Game.Player
import Game.Rules
import Game.View (GameStateView)

-- don't derive Functor so it's not easy to modify card nums
data NMResource = Chip | Card (Finite 35) deriving (Eq, Ord, Show, Read, Generic, Finitary)

instance ToJSON NMResource where
  toJSON r = String (T.pack . show $ r)

instance FromJSON NMResource where
  parseJSON (String s) = return . read . T.unpack $ s
  parseJSON _ = error $ "invalid JSON: NMResource"

extractCard :: NMResource -> Maybe Int
extractCard (Card i) = Just (fromEnum i)
extractCard _ = Nothing

isCard :: NMResource -> Bool
isCard = isJust . extractCard

cards :: [NMResource]
cards = filter isCard inhabitants

scoreCards :: Set NMResource -> Int
scoreCards = scoreCards' . S.map fromJust . S.filter isJust . S.map extractCard

-- could be fun to rewrite as a fold with accumulator like (sum, prev_element)
scoreCards' :: Set Int -> Int
scoreCards' cardValues = scoreSorted (S.toAscList cardValues) 0
  where
    scoreSorted (x : y : zs) currentScore =
      if y - x == 1
        then scoreSorted (y : zs) currentScore
        else scoreSorted (y : zs) (currentScore + x)
    scoreSorted [y] currentScore = currentScore + y
    scoreSorted [] currentScore = currentScore

data NMLocation
  = CenterOfTableCard
  | ChipPile
  | PlayerStuff Player
  | CardDeck
  | BoxTop
  deriving (Eq, Ord, Show, Generic, Finitary, FromJSON, ToJSON)

data NMCounters = DummyCounter deriving (Eq, Ord, Show, Generic, Enum, Bounded, FromJSON, ToJSON)

instance Finitary NMCounters

type NMGameObjects = GameObjects NMLocation NMCounters NMResource

initLocations' :: Set Player -> NMLocation -> LocationShape NMResource
initLocations' _ CenterOfTableCard = Slot Nothing
initLocations' _ ChipPile = Pile M.empty
initLocations' _ CardDeck = Deck (Seq.fromList cards)
initLocations' players (PlayerStuff player)
  | player `S.member` players = Pile (M.singleton Chip 11)
  | otherwise = Dummy
initLocations' _ BoxTop = Pile M.empty

initLocations :: Set Player -> FTMap NMLocation (LocationShape NMResource)
initLocations ps = FTMap (initLocations' ps)

initGameObjects :: Set Player -> NMGameObjects
initGameObjects ps =
  GameObjects
    { locations = initLocations ps,
      counters = FTMap (const dummyCounter)
    }

data NMPlayName = Take | Decline deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data NMPhaseName = Setup | NMTurnPhase Player deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

type NMTurn = Turn NMPhaseName

type NMPhase = Phase NMPhaseName NMLocation NMCounters NMResource NMPlayName

type NMGameState = GameState NMLocation NMCounters NMResource NMPhaseName NMPlayName

type NMOptions = Options NMPlayName

type NMGameRules = GameRules NMLocation NMCounters NMResource NMPhaseName NMPlayName

type NMM a = GameRule NMLocation NMCounters NMResource NMPhaseName NMPlayName a

type NMView = GameStateView NMLocation NMCounters NMResource NMPhaseName

type NMEvent = BEvent NMLocation NMCounters NMResource NMPhaseName NMPlayName

playerTurn :: Player -> NMTurn
playerTurn p = Turn p (NE.singleton (NMTurnPhase p))
