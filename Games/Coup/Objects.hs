{-# LANGUAGE DeriveAnyClass #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Objects where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Finitary
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as S
import FinitaryMap (FTMap (..))
import GHC.Generics (Generic)
import Game.Agent (BEvent)
import Game.GameState (GameRules, GameState, Phase)
import Game.Location
import Game.Options (Options)
import Game.Player
import Game.Rules (GameRule)
import Game.View (GameStateView)

-- | The five character roles. Each backs a claimed action or block.
data Role = Duke | Assassin | Captain | Ambassador | Contessa
  deriving (Eq, Ord, Show, Generic, Finitary, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

allRoles :: [Role]
allRoles = inhabitants

-- | Resources: coins and the role cards themselves.
data CoupResource = Coin | RoleCard Role
  deriving (Eq, Ord, Show, Generic, Finitary, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

extractRole :: CoupResource -> Maybe Role
extractRole (RoleCard r) = Just r
extractRole _ = Nothing

isRoleCard :: CoupResource -> Bool
isRoleCard = isJust . extractRole

-- | Where things live.
--
--   * 'CourtDeck'    — the face-down draw deck of role cards (hidden from all).
--   * 'Treasury'     — an infinite supply of coins.
--   * 'Influence' p  — p's face-down cards (only p may see them).
--   * 'Revealed'  p  — p's lost, face-up cards (public).
--   * 'Coins'     p  — p's coin pile (public).
--   * 'ExchangeZone' — scratch space for the Ambassador exchange.
data CoupLocation
  = CourtDeck
  | Treasury
  | Influence Player
  | Revealed Player
  | Coins Player
  | ExchangeZone
  deriving (Eq, Ord, Show, Generic, Finitary, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

extractPlayer :: CoupLocation -> Maybe Player
extractPlayer (Influence p) = Just p
extractPlayer (Revealed p) = Just p
extractPlayer (Coins p) = Just p
extractPlayer _ = Nothing

-- | Every atomic decision a player can be asked to make. At each decision
-- point only the legal subset is offered (via 'Options'); the play runner
-- treats reaction/sub-choice plays as no-ops and the orchestration code
-- branches on the returned value.
data CoupPlayName
  = -- main actions (chosen by the active player)
    Income
  | ForeignAid
  | TakeTax
  | LaunchCoup Player
  | Assassinate Player
  | Steal Player
  | ExchangeCards
  | -- challenge window. NOTE: options are presented in derived-Ord order, so
    -- 'AllowIt' is listed BEFORE 'Challenge' to keep "proceed / don't
    -- intervene" as key 1 in every reaction prompt (challenge and block).
    AllowIt
  | Challenge
  | -- block claims
    BlockForeignAid
  | BlockStealCaptain
  | BlockStealAmbassador
  | BlockAssassination
  | -- which influence card to give up
    Reveal Role
  deriving (Eq, Ord, Show, Generic, Finitary, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

-- | One phase per player turn.
data CoupPhaseName = CoupTurn Player
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

-- Initialization --------------------------------------------------------------

type CoupGameObjects = GameObjects CoupLocation NoCounters CoupResource

-- | Three copies of each role make the 15-card court deck.
courtDeckCards :: [CoupResource]
courtDeckCards = concatMap (replicate 3 . RoleCard) allRoles

startingCoins :: Int
startingCoins = 2

initLocations' :: Set Player -> CoupLocation -> LocationShape CoupResource
initLocations' _ CourtDeck = deckOf courtDeckCards
initLocations' _ Treasury = infinite Coin
initLocations' _ ExchangeZone = emptyPile
initLocations' players (Coins p)
  | p `S.member` players = pileOf Coin startingCoins
  | otherwise = dummy
initLocations' players (Influence p)
  | p `S.member` players = emptyPile -- dealt during setup
  | otherwise = dummy
initLocations' players (Revealed p)
  | p `S.member` players = emptyPile
  | otherwise = dummy

initLocations :: Set Player -> FTMap CoupLocation (LocationShape CoupResource)
initLocations ps = FTMap (initLocations' ps)

initGameObjects :: Set Player -> CoupGameObjects
initGameObjects ps =
  GameObjects
    { locations = initLocations ps,
      counters = FTMap (const dummyCounter)
    }

-- Type aliases ----------------------------------------------------------------

type CoupTurn = Turn CoupPhaseName

type CoupPhase = Phase CoupPhaseName CoupLocation NoCounters CoupResource CoupPlayName

type CoupGameState = GameState CoupLocation NoCounters CoupResource CoupPhaseName CoupPlayName

type CoupOptions = Options CoupPlayName

type CoupGameRules = GameRules CoupLocation NoCounters CoupResource CoupPhaseName CoupPlayName

type CoupM a = GameRule CoupLocation NoCounters CoupResource CoupPhaseName CoupPlayName a

type CoupView = GameStateView CoupLocation NoCounters CoupResource CoupPhaseName

type CoupEvent = BEvent CoupLocation NoCounters CoupResource CoupPhaseName CoupPlayName

-- | The turn structure for a player: a single 'CoupTurn' phase.
playerTurn :: Player -> CoupTurn
playerTurn p = Turn p (NE.singleton (CoupTurn p))
