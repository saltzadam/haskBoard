{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveAnyClass #-}
module Objects 
    where
import GHC.Generics (Generic)
import Game.Player
import Game.Location
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S
import Game.GameNode (GameAction, GameNode)
import FinitaryMap (FTMap(..))
import qualified Data.Sequence as Seq
import qualified Data.List.NonEmpty as NE
import Data.Finitary
import Game.GameState (Turn(..), Phase, GameState, GameRules)
import Game.Monad (GameEff)
import Game.Options (Options)
import Data.Maybe (isJust, fromJust)
import Game.View (GameStateView)

-- don't derive Functor so it's not easy to modify card nums
data NMResource = Chip | Card Int deriving (Eq, Ord, Show, Generic, Finitary)

cards :: [NMResource]
cards = [Card i | i <- [3 .. 35]]

-- TODO: try lens
extractCard :: NMResource -> Maybe Int
extractCard (Card i) = Just i
extractCard _ = Nothing

isCard :: NMResource -> Bool
isCard = isJust . extractCard 
-- TODO: ugly
scoreCards :: Set NMResource -> Int
scoreCards = scoreCards' . S.map fromJust . S.filter isJust . S.map extractCard

scoreCards' :: Set Int -> Int
scoreCards' cardValues = scoreSorted (S.toAscList cardValues) 0
    where
        scoreSorted (x:y:zs) currentScore = if y - x == 1
                                            then scoreSorted (y:zs) currentScore
                                            else scoreSorted (y:zs) (currentScore + x)
        scoreSorted [y] currentScore = currentScore + y
        scoreSorted [] currentScore = currentScore


data NMLocation
  = CenterOfTableCard
  | ChipPile
  | PlayerStuff Player
  | CardDeck
  | BoxTop
  deriving (Eq, Ord, Show, Generic, Finitary)


data NMCounters = DummyCounter deriving (Eq, Ord, Show, Generic, Enum, Bounded)
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

data NMIssue = NoMoreChips deriving (Eq, Ord, Show, Generic)
data NMPlayName = Take | Decline deriving (Eq, Ord, Show, Generic)
data NMPhaseName = Setup | NMTurn Player deriving (Eq, Ord, Show, Generic)
type NMTurn = Turn NMPhaseName
type NMPhase = Phase NMPhaseName NMLocation NMCounters NMResource NMPlayName NMIssue
-- type NMAction = GameAction NMLocation NMCounters NMResource NMPhaseName
type NMGameState = GameState NMLocation NMCounters NMResource NMPhaseName NMPlayName NMIssue

type NMOptions = Options NMPlayName NMIssue

type NMGameRules = GameRules  NMLocation NMCounters NMResource NMPhaseName NMPlayName NMIssue
type NMGameNode = GameNode NMLocation NMCounters NMResource NMPhaseName NMPlayName NMIssue

type NMM a = GameEff NMLocation NMCounters NMResource NMPhaseName NMPlayName NMIssue a

type NMView  = GameStateView NMLocation NMCounters NMResource NMPhaseName 

currentPlayer :: NMPhaseName -> Maybe Player
currentPlayer (NMTurn p) = Just p
currentPlayer _ = Nothing

playerTurn :: Player -> NMTurn
playerTurn p = Turn p (NE.singleton (NMTurn p))

