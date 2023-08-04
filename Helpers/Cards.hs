module Cards where

import Game.Rules
import Helpers

draw :: Eq l => l -> l -> GameRule l cn r ph pl i ()
draw hand deck = do
  topCard <- peek deck
  case topCard of
    Just card -> transfer deck hand card
    Nothing -> justDoNothing
