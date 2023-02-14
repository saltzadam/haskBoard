{-# LANGUAGE OverloadedLabels #-}
module Tui
    where

import Brick (App(..), BrickEvent(..), neverShowCursor, EventM, AttrMap, (<+>), (<=>))
import Game (Game(..))
import Brick.Types (Widget)
import Control.Lens ((^.))


type Name = ()

app :: App (Game l cn r ph pl t pls) e Name
app = App {   appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = const theAttrMap
          }

drawUI :: Game l cn r ph pl t pls -> [Widget Name]
drawUI g = [ drawBoard g <+> (drawDice g <=> drawMenu g)]

drawBoard :: Game l cn r ph pl t pls -> Widget Name
drawBoard g = undefined

drawDice :: Game l cn r ph pl t pls -> Widget Name
drawDice g = let
    diceVals = g ^. #objects . #counters
              in undefined

drawMenu :: Game l cn r ph pl t pls -> Widget Name
drawMenu g = undefined

handleEvent :: BrickEvent Name e -> EventM Name (Game l cn r ph pl t pls) ()
handleEvent = undefined

theAttrMap :: AttrMap
theAttrMap = undefined
