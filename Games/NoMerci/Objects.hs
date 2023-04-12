{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Objects 
    where
import GHC.Generics (Generic)
import Game.Player
import Location
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S
import GameE (Phase, GameState, Game, ObserveGame, Turn (..))
import GameNode (GameAction, GameNode)
import FinitaryMap (FTMap(..))
import qualified Data.Sequence as Seq
import qualified Data.List.NonEmpty as NE
import Data.Finitary

-- data CenterOfTable = CenterOfTable deriving (Eq, Ord, Show, Enum, Generic)
-- instance Finitary CenterOfTable 

-- data ChipPile = ChipPile deriving (Eq, Ord, Show, Enum, Generic)
-- instance Finitary ChipPile 

-- don't derive Functor so it's not easy to modify card nums
data NMResource = Chip | Card Int deriving (Eq, Ord, Show, Generic)

isCard :: NMResource -> Bool
isCard (Card _) = True
isCard _ = False

data NMLocation
  = CenterOfTableCard
  | ChipPile
  | PlayerStuff Player
  | CardDeck
  deriving (Eq, Ord, Show, Generic)

data NMCounters = DummyCounter deriving (Eq, Ord, Show, Generic)
instance Finitary NMCounters

type NMGameObjects = GameObjects NMLocation NMCounters NMResource

initLocations' :: Set Player -> NMLocation -> LocationShape NMResource
initLocations' _ CenterOfTableCard = Slot Nothing
initLocations' _ ChipPile = Pile M.empty
initLocations' _ CardDeck = Deck (Seq.fromList [Card i | i <- [3..35]])
initLocations' players (PlayerStuff player)
    | player `S.member` players = Pile (M.singleton Chip 11)
    | otherwise = Dummy

initLocations :: Set Player -> FTMap NMLocation (LocationShape NMResource)
initLocations ps = FTMap (initLocations' ps)

initGameObjects :: Set Player -> NMGameObjects
initGameObjects ps =
  GameObjects
    { locations = initLocations ps,
      counters = FTMap (const dummyCounter)
    }


data Issue = NoMoreChips deriving (Eq, Ord, Show, Generic)
data NMPlayName = Take | Decline deriving (Eq, Ord, Show, Generic)
data NMPhaseName = NMTurn Player deriving (Eq, Ord, Show, Generic)
type NMTurn = Turn NMPhaseName
type NMPhase = Phase NMPhaseName NMLocation NMCounters NMResource NMPlayName Issue
type NMAction = GameAction NMLocation NMCounters NMResource NMPhaseName


type NMGameState = GameState NMLocation NMCounters NMResource NMPhaseName NMPlayName Issue
type NMGame = Game  NMLocation NMCounters NMResource NMPhaseName NMPlayName Issue
type NMGameNode = GameNode NMLocation NMCounters NMResource NMPhaseName NMPlayName Issue
type Observe es = ObserveGame NMLocation NMCounters NMResource NMPhaseName NMPlayName Issue es

currentPlayer :: NMPhaseName ->Player
currentPlayer (NMTurn p) = p

playerTurn :: Player -> NMTurn
playerTurn p = Turn p (NE.singleton (NMTurn p))

