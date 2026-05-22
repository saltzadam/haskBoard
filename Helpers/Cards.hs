module Cards where

import Control.Monad (replicateM_)
import Data.Foldable (traverse_)
import Game.Location (LocationShape (..))
import Game.Player (Player)
import Game.Rules
import Helpers

-- | Transfer the top card from a deck to a location.
draw :: (Eq l, Show r) => l -> l -> GameRule l cn r ph pl ()
draw deck hand = do
  topCard <- peek deck
  case topCard of
    Just card -> transfer deck hand card
    Nothing -> justDoNothing

-- | Transfer a specific resource from one location to another.
play :: l -> l -> r -> GameRule l cn r ph pl ()
play = transfer

-- | Draw N cards from a deck to a location.
drawN :: (Eq l, Show r) => Int -> l -> l -> GameRule l cn r ph pl ()
drawN n deck hand = replicateM_ n (draw deck hand)

-- | Draw all cards from a deck to a location (empties the source deck).
drawAll :: (Eq l, Ord r, Show r) => l -> l -> GameRule l cn r ph pl ()
drawAll deck hand = do
  c <- peek deck
  case c of
    Nothing -> justDoNothing
    Just _  -> draw deck hand >> drawAll deck hand

-- | Look at the top N cards of a deck without moving them.
peekN :: (Eq l) => Int -> l -> GameRule l cn r ph pl [r]
peekN n loc = do
  shape <- lookLocation loc
  case shape of
    Deck s -> return . take n . foldr (:) [] $ s
    _      -> return []

-- | Deal one card from a deck to each of the given players.
dealTo :: (Eq l, Show r) => l -> (Player -> l) -> [Player] -> GameRule l cn r ph pl ()
dealTo deck playerLoc = traverse_ (\p -> draw deck (playerLoc p))

-- | Deal N cards from a deck to each of the given players (round-robin).
dealNTo :: (Eq l, Show r) => Int -> l -> (Player -> l) -> [Player] -> GameRule l cn r ph pl ()
dealNTo n deck playerLoc players = replicateM_ n (dealTo deck playerLoc players)
