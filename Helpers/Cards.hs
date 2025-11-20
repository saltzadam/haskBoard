module Cards where

import Game.Rules
import Helpers

draw :: (Eq l, Show r) => l -> l -> GameRule l cn r ph pl ()
draw deck hand = do
  topCard <- peek deck
  case topCard of
    Just card -> transfer deck hand card
    Nothing -> justDoNothing

play :: l -> l -> r -> GameRule l cn r ph pl ()
play = transfer
