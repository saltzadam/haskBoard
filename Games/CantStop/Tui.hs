{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Tui
    where

import Brick (App(..), BrickEvent(..), neverShowCursor, EventM, AttrMap, attrMap, on, AttrName, withAttr, str, attrName, hBox, Padding (..), padLeftRight, (<+>), vLimitPercent, (<=>), fill, halt)
import Brick.Types (Widget)
import Control.Lens ((^.), view)
import Game.Player (Player (..))
import qualified Graphics.Vty as V
import Game.Location (listAllShape)
import Brick.Widgets.Table (table, Table, rowBorders, columnBorders, surroundingBorder, renderTable, ColumnAlignment (..), setDefaultColAlignment, RowAlignment (..), setDefaultRowAlignment)
import Brick.Widgets.Core (padTop)
import Brick.Widgets.Border
import FinitaryMap ((!!!), inhabitants)
import Dice (renderDice)
import Brick.Widgets.Center
import Objects
import Game.View (GameStateView(..), getHintView, locationView)
import qualified Data.Text as T


type Name = ()

app :: App CantStopGameStateView e Name
app = App {   appDraw = drawUIView
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = const theAttrMap
          }

drawUIView :: CantStopGameStateView -> [Widget Name]
drawUIView g = [drawBoardView g <+> (drawDice (objectsView g) <=> border (fill '/'))] -- [ drawBoard g <+> (drawDice g <=> drawMenu g)]

drawBoardView :: CantStopGameStateView -> Widget Name
drawBoardView gsv = border $ padTop Max . renderTable . boxTable $ [drawVerticalTrack gsv <$> [Two .. Twelve], str . show <$> [2 .. 12] ]


drawDice :: CantStopGameObjectsView -> Widget Name
drawDice g = let
    dice = g ^. #countersView
    maybeDiceVals' = traverse (fmap (view #val) . (dice !!!)) (inhabitants :: [CantStopCounterName])
    maybeDiceVals = fmap (\diceVals'' ->  [[diceVals'' !! 0, diceVals'' !! 1], [diceVals'' !! 2, diceVals'' !! 3]]) maybeDiceVals'
    renderFn =  fmap (fmap (padLeftRight 1 . str . renderDice)) 
    formatFn =  vLimitPercent 40 . border .  center . renderTable . boxTable
              in case maybeDiceVals of
                   Just dv -> formatFn . renderFn $ dv
                   Nothing -> formatFn . renderFn $ []

drawMenu :: CantStopGame -> Widget Name
drawMenu g =undefined

handleEvent :: BrickEvent Name e -> EventM Name CantStopGameStateView ()
handleEvent e = case e of
    VtyEvent (V.EvKey V.KEsc []) -> halt
    _ -> pure ()

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
playerToColor (Player i) = playerAttrs !! fromIntegral i

boxTable :: [[Widget n]] -> Table n
boxTable = rowBorders False. columnBorders False . surroundingBorder False
        . setDefaultRowAlignment AlignBottom . setDefaultColAlignment AlignCenter
        . table
square :: Widget Name
square = str "\x02588"

drawPiece :: CantStopResource -> Widget Name
drawPiece (PlayerMarker p) = padTop (Pad 1) . withAttr (playerToColor p) $ square
drawPiece TemporaryMarker = padTop (Pad 1) . withAttr tempMarkAttr $ square

drawSlot :: CantStopGameStateView -> CantStopLocation -> Widget Name
drawSlot gsv t@(TrackSpot _ _) = let
    pieces = listAllShape <$> (gsv ^. locationView t)
      in case pieces of
        Nothing -> padTop (Pad 1) square
        (Just []) -> padTop (Pad 1) square
        Just pieces' -> if null pieces'
                        then padTop (Pad 1) square
                        else
                            hBox (fmap drawPiece pieces')
drawSlot _ _ = error "can only draw TrackSpot"


drawVerticalTrack :: CantStopGameStateView -> TrackName -> Widget Name
drawVerticalTrack gsv track = formatTrack  $
                            case getHintView (T.pack $ show track ++ "Winner") gsv  of
                              Just pText -> let p = read (T.unpack pText) :: Player
                                            in fmap (const (drawPiece (PlayerMarker p))) <$> spots
                              Nothing -> fmap (drawSlot gsv) <$> spots
                            where
                                formatTrack = padLeftRight 3 .  renderTable . boxTable
                                spots = reverse [[TrackSpot track i] | i <- [HOne .. maxSlot track]]

