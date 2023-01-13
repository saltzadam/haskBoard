{-# LANGUAGE OverloadedLabels #-}
module Game.Control where
import Game
import Game.Player
import Control.Lens (view)
import Data.Maybe (listToMaybe)
import Data.List (elemIndex)
import Data.Tree as Tree

data Decision l cn r ph pl t tn = Decision deriving (Show)

type TestEventNodes l cn r ph pl t tn = Either (Decision l cn r ph pl t tn) (GameAction l cn r ph)

type TestEventTree l cn r ph pl t tn = Tree.Tree (TestEventNodes l cn r ph pl t tn)


