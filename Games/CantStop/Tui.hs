{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Tui
    where

import Brick (App(..), BrickEvent(..), neverShowCursor, EventM, AttrMap, attrMap, on, AttrName, withAttr, str, attrName, hBox, Padding (..), padLeftRight, (<+>), vLimitPercent, (<=>), fill)
import Brick.Types (Widget)
import Control.Lens ((^.), view)
import Game.Player (Player (..))
import CantStop (CantStopLocation (..), CantStopResource (..), maxSlot, TrackName (..), TrackHeight(..), trackWinners, CantStopGame, CantStopCounterName)
import qualified Graphics.Vty as V
import Location (listAll)
import Brick.Widgets.Table (table, Table, rowBorders, columnBorders, surroundingBorder, renderTable, ColumnAlignment (..), setDefaultColAlignment, RowAlignment (..), setDefaultRowAlignment)
import Brick.Widgets.Core (padTop)
import Brick.Widgets.Border
import FinitaryMap ((!!!), FTMap)
import Data.Finitary
import Dice (renderDice)
import Brick.Widgets.Center
import qualified Data.Map as M
import GameE (observe)
import Count


type Name = ()

app :: App CantStopGame e Name
app = App {   appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = const theAttrMap
          }

drawUI :: CantStopGame -> [Widget Name]
drawUI g = [drawBoard g <+> (drawDice g <=> border (fill '/'))] -- [ drawBoard g <+> (drawDice g <=> drawMenu g)]

drawBoard :: CantStopGame -> Widget Name
drawBoard g = border $ padTop Max . renderTable . boxTable $ [drawVerticalTrack g <$> [Two .. Twelve], str . show <$> [2 .. 12] ]


drawDice :: CantStopGame -> Widget Name
drawDice g = let
    dice = g ^. #gameState . #objects . #counters 
    diceVals = fmap (view #val . (dice !!!)) (inhabitants :: [CantStopCounterName])
              in vLimitPercent 40 . border .  center . renderTable . boxTable $ fmap (fmap (padLeftRight 1 . str . renderDice)) [[diceVals !! 0, diceVals !! 1], [diceVals !! 2, diceVals !! 3]]

drawMenu :: CantStopGame -> Widget Name
drawMenu g =undefined

handleEvent :: BrickEvent Name e -> EventM Name CantStopGame ()
handleEvent = undefined

player0Attr, player1Attr, player2Attr, player3Attr :: AttrName
player0Attr = attrName "player0"
player1Attr = attrName "player1"
player2Attr = attrName "player2"
player3Attr = attrName "player3"

playerAttrs :: [AttrName]
playerAttrs = [player0Attr, player1Attr, player2Attr, player3Attr]

emptyAttr :: AttrName
emptyAttr = attrName "empty"

tempMarkAttr :: AttrName
tempMarkAttr = attrName "tempMark"

theAttrMap :: AttrMap
theAttrMap = attrMap (V.brightRed `on` V.black)
    [ (player0Attr, V.red `on` V.black),
      (player1Attr, V.blue `on` V.black),
      (player2Attr, V.green `on` V.black),
      (player3Attr, V.yellow `on` V.black),
      (tempMarkAttr, V.white `on` V.black),
      (emptyAttr, V.rgbColor 160 160 160  `on` V.black)
    ]

playerToColor :: Player -> AttrName
playerToColor (Player i) = playerAttrs !! fromIntegral (i)

boxTable :: [[Widget n]] -> Table n
boxTable = rowBorders False. columnBorders False . surroundingBorder False
        . setDefaultRowAlignment AlignBottom . setDefaultColAlignment AlignCenter
        . table
square :: Widget Name
square = str "\x02588"

drawPiece (PlayerMarker p) = padTop (Pad 1) . withAttr (playerToColor p) $ square
drawPiece TemporaryMarker = padTop (Pad 1) . withAttr tempMarkAttr $ square

drawSlot :: CantStopGame -> CantStopLocation -> Widget Name
drawSlot gs t@(TrackSpot _ _) = let
    pieces = listAll t (gs ^. #gameState . #objects . #locations)
    in
        if null pieces then padTop (Pad 1) square
        else
            hBox (fmap drawPiece pieces)
drawSlot _ _ = error "can only draw TrackSpot"

drawVerticalTrack :: CantStopGame -> TrackName -> Widget Name
drawVerticalTrack gs track = formatTrack  $
                            case M.lookup track (observe gs trackWinners) of
                              Just p -> fmap (const (drawPiece (PlayerMarker p))) <$> spots
                              Nothing -> fmap (drawSlot gs) <$> spots
                            where
                                formatTrack = padLeftRight 3 .  renderTable . boxTable  
                                spots = reverse [[TrackSpot track i] | i <- [HOne .. maxSlot track]]

