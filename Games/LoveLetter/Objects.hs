{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE KindSignatures #-}

module Objects where

import Data.Finitary
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import Data.Set (Set)
import Data.Void (Void, absurd)
import FinitaryMap (FTMap (..))
import GHC.Generics (Generic)
import Game.GameState
import Game.Location
import Game.Options (Legality (..), Options (..))
import Game.Player
import Game.Rules
import Game.View (GameStateView)
import Util (buildSafeNonempty)

data Character
  = Guard
  | Priest
  | Baron
  | Handmaid
  | Prince
  | King
  | Countess
  | Princess
  deriving (Eq, Ord, Show, Generic, Enum, Bounded, Finitary)

characters :: NonEmpty Character
characters = NE.fromList inhabitants

charStrength :: Character -> Int
charStrength char = fromEnum char + 1

startingCards :: [LLResource]
startingCards =
  Card
    <$> [ Princess,
          Countess,
          King,
          Prince,
          Prince,
          Handmaid,
          Handmaid,
          Baron,
          Baron,
          Priest,
          Priest,
          Guard,
          Guard,
          Guard,
          Guard,
          Guard
        ]

data LLResource = Token | HandmaidMarker | Card Character deriving (Eq, Ord, Show, Generic, Finitary)

cards :: [LLResource]
cards = Card <$> inhabitants

extractChar :: LLResource -> Maybe Character
extractChar (Card char) = Just char
extractChar _ = Nothing

data LLLocation
  = PlayedCard
  | TheDeck
  | Hand Player
  | HandmaidInd Player
  | Tokens Player
  | BoxTop
  deriving (Eq, Ord, Show, Generic, Finitary)

type LLCounters = Void

instance Enum Void

type LLGameObjects = GameObjects LLLocation LLCounters LLResource

initLocations' :: Set Player -> LLLocation -> LocationShape LLResource
initLocations' _ PlayedCard = Slot Nothing
initLocations' _ TheDeck = Deck (Seq.fromList startingCards)
initLocations' _ (Tokens _) = Pile M.empty
initLocations' _ (Hand _) = Pile M.empty
initLocations' _ (HandmaidInd _) = Slot Nothing
initLocations' _ BoxTop = Pile $ M.fromList [(Token, 16), (HandmaidMarker, 5)]

initLocations :: Set Player -> FTMap LLLocation (LocationShape LLResource)
initLocations = FTMap . initLocations'

initGameObjects :: Set Player -> LLGameObjects
initGameObjects ps =
  GameObjects
    { locations = initLocations ps,
      counters = FTMap absurd
    }

data LLIssue = ProtectedByHandmaid | MustDiscardCountess | OtherValidTarget deriving (Eq, Ord, Show, Generic)

data LLPlayName
  = PlayPrincess
  | PlayCountess
  | PlayKing Player
  | PlayPrince Player
  | PlayHandmaid
  | PlayBaron Player
  | PlayPriest Player
  | PlayGuard Player Character
  deriving (Eq, Ord, Show, Generic)

playGetter :: (Applicative m) => Character -> m Player -> m Character -> m LLPlayName
playGetter Princess _ _ = pure PlayPrincess
playGetter Countess _ _ = pure PlayCountess
playGetter King playerGetter _ = PlayKing <$> playerGetter
playGetter Prince playerGetter _ = PlayPrince <$> playerGetter
playGetter Handmaid _ _ = pure PlayHandmaid
playGetter Baron playerGetter _ = PlayBaron <$> playerGetter
playGetter Priest playerGetter _ = PlayPriest <$> playerGetter
playGetter Guard playerGetter charGetter = PlayGuard <$> playerGetter <*> charGetter

target :: LLPlayName -> Maybe Player
target (PlayKing p) = Just p
target (PlayPrince p) = Just p
target (PlayBaron p) = Just p
target (PlayPriest p) = Just p
target (PlayGuard p _) = Just p
target _ = Nothing

playToCharacter :: LLPlayName -> Character
playToCharacter PlayPrincess = Princess
playToCharacter PlayCountess = Countess
playToCharacter (PlayKing _) = King
playToCharacter (PlayPrince _) = Prince
playToCharacter PlayHandmaid = Handmaid
playToCharacter (PlayBaron _) = Baron
playToCharacter (PlayPriest _) = Priest
playToCharacter (PlayGuard _ _) = Guard

data LLPhaseName = Setup | LLTurn Player deriving (Eq, Ord, Show, Generic)

type LLTurn = Turn LLPhaseName

type LLPhase = Phase LLPhaseName LLLocation LLCounters LLResource LLPlayName LLIssue

type LLGameState = GameState LLLocation LLCounters LLResource LLPhaseName LLPlayName LLIssue

type LLOptions = Options LLPlayName LLIssue

type LLGameRules = GameRules LLLocation LLCounters LLResource LLPhaseName LLPlayName LLIssue

type LLM a = GameRule LLLocation LLCounters LLResource LLPhaseName LLPlayName LLIssue a

type LLView = GameStateView LLLocation LLCounters LLResource LLPhaseName

-- TODO: lens
currentPlayer :: LLPhaseName -> Maybe Player
currentPlayer (LLTurn p) = Just p
currentPlayer _ = Nothing

playerTurn :: Player -> LLTurn
playerTurn p = Turn p (NE.singleton (LLTurn p))
