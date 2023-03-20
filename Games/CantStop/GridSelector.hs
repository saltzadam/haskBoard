{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module GridSelector where

import Brick
    ( Named(..),
      EventM,
      Widget,
      attrName,
      continueWithoutRedraw,
      modify,
      AttrName )
import Brick.Widgets.Table ( renderTable, Table, rowBorders, columnBorders, surroundingBorder, table )
import Control.Lens (Contravariant (..), makeLenses, over, set)
import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Maybe (mapMaybe)
import Graphics.Vty (Event)
import qualified Graphics.Vty.Input as V
import Data.List ((\\))


tableNoBorders :: [[Brick.Widget n]] -> Table n
tableNoBorders =
  rowBorders False
    . columnBorders False
    . surroundingBorder False
    . table

toggleInList :: Eq a => a -> [a] -> [a]
toggleInList x xs = if x `elem` xs then xs \\ [x] else x : xs

-- selector widget with name of type n, items of type e dervied from state of type s
data GridSelector n e s = GridSelector
  { _gridName :: n,
    -- | inner lists are rows
    _gridItems :: s -> [[Maybe e]],
    _gridHighlighted :: !(Maybe (Int, Int)), -- think this is a classic example of not actually being strict!!
    _gridSelected :: ![(Int, Int)], -- switch to set
    _gridSelectionMax :: Maybe Int,
    _gridItemHeight :: Int,
    _gridForbidden :: ![(Int, Int)], -- switch to set
    _gridRows :: Int,
    _gridColumns :: Int
  }

instance Contravariant (GridSelector n e) where
  contramap proj grid = grid {_gridItems = newGridItems}
    where
      newGridItems = _gridItems grid . proj

makeLenses ''GridSelector

instance Named (GridSelector n e s) n where
  getName = _gridName

-- Handle Events --

gridItemAttr :: AttrName
gridItemAttr = attrName "gridItem"

gridSelectedAttr :: AttrName
gridSelectedAttr = attrName "gridSelected"

gridHighlightedAttr :: AttrName
gridHighlightedAttr = attrName "gridHighlighted"

data GridItemState = Highlighted | Selected | Normal deriving (Eq, Show, Ord)

-- don't just index visible items
indexGrid :: [[a]] -> [[((Int, Int), a)]]
indexGrid grid = fmap go . zip [1 ..] . fmap (zip [1 ..]) $ grid
  where
    go :: (Int, [(Int, a)]) -> [((Int, Int), a)]
    go = fmap (\(a, (b, c)) -> ((b, a), c)) . sequence

-- note transposition -- this makes the index work like (x,y)

getSelected :: s -> GridSelector n e s -> [((Int, Int), Maybe e)]
getSelected state grid = filter (\(ind, _) -> ind `elem` _gridSelected grid) . concat . indexGrid $ _gridItems grid state

getSelectedItems :: s -> GridSelector n e s -> [e]
getSelectedItems s = mapMaybe snd . getSelected s

-- doesn't actually need s!!
getSelectedIndices :: s -> GridSelector n e s -> [(Int, Int)]
getSelectedIndices s = fmap fst . mapMaybe sequence . getSelected s

renderGridSelector ::
  forall n e s.
  Show e =>
  -- | rendering function
  (GridItemState -> Maybe e -> Widget n) ->
  -- | is the grid focused?
  Bool ->
  -- | for when it's focused
  AttrName ->
  -- | state
  s ->
  GridSelector n e s ->
  Widget n
renderGridSelector drawGridElem isFoc attr s grid =
  renderTable . tableNoBorders $ drawn
  where
    padRow :: [Maybe e] -> [Maybe e]
    padRow row = row ++ replicate (length row - _gridColumns grid) Nothing
    rows = padRow <$> _gridItems grid s :: [[Maybe e]] -- TODO: add padding on rows too!
    indexed = indexGrid rows
    drawElemWithState ((i, j), e) = drawGridElem (gridState (i, j)) e
    drawn = fmap (fmap drawElemWithState) indexed

    gridState :: (Int, Int) -> GridItemState
    gridState (i, j)
      | Just (i, j) == _gridHighlighted grid = Highlighted
      | (i, j) `elem` _gridSelected grid = Selected
      | otherwise = Normal

clearSelection :: GridSelector n e s -> GridSelector n e s
clearSelection = set gridHighlighted Nothing . set gridSelected []

gridMoveBy :: (Int, Int) -> GridSelector n e s -> GridSelector n e s
gridMoveBy (x, y) grid = if _gridHighlighted result `elem` (fmap Just . _gridForbidden $ result) then grid else result
  where
    -- need to do 1-indexed modular arithmetic
    addMod modulo i j = ((i + j - 1) `mod` modulo) + 1
    lenY = _gridRows grid :: Int
    lenX = _gridColumns grid :: Int
    result = over gridHighlighted (fmap (bimap (addMod lenX x) (addMod lenY y))) grid

gridMoveUp :: GridSelector n e s -> GridSelector n e s
gridMoveUp = gridMoveBy (0, -1)

gridMoveDown :: GridSelector n e s -> GridSelector n e s
gridMoveDown = gridMoveBy (0, 1)

gridMoveLeft :: GridSelector n e s -> GridSelector n e s
gridMoveLeft = gridMoveBy (-1, 0)

gridMoveRight :: GridSelector n e s -> GridSelector n e s
gridMoveRight = gridMoveBy (1, 0)

-- simplify -- note that there are only two expressions!
toggleSelection :: GridSelector n e s -> GridSelector n e s
toggleSelection grid = case _gridHighlighted grid of
  Nothing -> grid
  Just highl -> case _gridSelectionMax grid of
    Nothing -> grid {_gridSelected = toggleInList highl (_gridSelected grid)}
    Just limit ->
      if (length (_gridSelected grid) < limit) || (highl `elem` _gridSelected grid)
        then grid {_gridSelected = toggleInList highl (_gridSelected grid)}
        else grid

-- avoid using gamestate here -- goal is to send an event back to controller
handleGridEvent :: Event -> EventM n (GridSelector n e s) ()
handleGridEvent e =
  case e of
    V.EvKey V.KUp [] -> modify gridMoveUp
    V.EvKey V.KDown [] -> modify gridMoveDown
    V.EvKey V.KRight [] -> modify gridMoveRight
    V.EvKey V.KLeft [] -> modify gridMoveLeft
    V.EvKey (V.KChar ' ') [] -> modify toggleSelection
    _ -> continueWithoutRedraw
