{-# LANGUAGE OverloadedLabels #-}
module Game.Control where
import Game
import Game.Player
import Control.Lens (view)
import Data.Maybe (listToMaybe)
import Data.List (elemIndex)
import Data.Tree as Tree
import Control.Monad.Trans.State

nextCyclic :: Game l cn r ph pl t tn -> Maybe Player
nextCyclic g = case view #activePlayer g of
                 Nothing -> listToMaybe (view #players g)
                 Just p -> findNextCyclic p (view #players g)

findNextCyclic :: Eq a => a -> [a] -> Maybe a
findNextCyclic x xs = let
    ind = (+1) <$> elemIndex x xs 
    in
        if ind == Just (length xs) 
        then listToMaybe xs 
        else (xs !!) <$> ind 

data Decision l cn r ph pl t tn = Decision deriving (Show)

type TestEventNodes l cn r ph pl t tn = Either (Decision l cn r ph pl t tn) (GameAction l cn r ph)

type TestEventTree l cn r ph pl t tn = Tree.Tree (TestEventNodes l cn r ph pl t tn)





