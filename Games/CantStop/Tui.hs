{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Tui
    where

import Brick (App(..), BrickEvent(..), neverShowCursor, EventM, AttrMap, attrMap, on, AttrName, withAttr, str, attrName, hBox, Padding (..), padLeftRight, (<+>), vLimitPercent, (<=>), fill, halt, put, txt, txtWrap, padBottom)
import Brick.Types (Widget)
import Control.Lens ((^.), view, assign)
import Game.Player (Player (..))
import qualified Graphics.Vty as V
import Game.Location (listAllShape)
import Brick.Widgets.Table (table, Table, rowBorders, columnBorders, surroundingBorder, renderTable, ColumnAlignment (..), setDefaultColAlignment, RowAlignment (..), setDefaultRowAlignment)
import Brick.Widgets.Core (padTop)
import Brick.Widgets.Border
import FinitaryMap ((!!!), inhabitants, ftAt)
import Dice (renderDice)
import Brick.Widgets.Center
import Objects
import qualified Data.Text as T
import Game.Helpers (viewCounterVal, viewCounterValC)
import GHC.Generics (Generic)
import Draw
import Control.Lens.TH (makeFields)


type Name = ()

data BEvent = Receive CSView 
            | Request CantStopOptions
            | Answer PlayName
            deriving (Generic)

data TUIState = TUIState { gameStateViewC :: CSView
                        , viewer :: Player
                        , lastEvent :: BEvent
                         } deriving (Generic)

makeFields 'TUIState

app :: App TUIState BEvent Name
app = App {   appDraw = drawUIView
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = const theAttrMap
          }

drawUIView :: TUIState -> [Widget Name]
drawUIView tui@(TUIState g _ _ ) = [drawBoardView g <+> (drawDice g  <=> drawMenu tui)] -- [ drawBoard g <+> (drawDice g <=> drawMenu g)]

drawBoardView :: CSView -> Widget Name
drawBoardView gsv = border $ padTop Max . renderTable . boxTable $ [drawVerticalTrack gsv <$> [Two .. Twelve], str . show <$> [2 .. 12] ]


drawDice :: CSView -> Widget Name
drawDice g = let
    maybeDiceVals' = traverse (`viewCounterValC` g)  (inhabitants :: [CantStopCounterName])
    maybeDiceVals = fmap (\diceVals'' ->  [[diceVals'' !! 0, diceVals'' !! 1], [diceVals'' !! 2, diceVals'' !! 3]]) maybeDiceVals'
    renderFn =  fmap (fmap (padLeftRight 1 . str . renderDice)) 
    formatFn =  vLimitPercent 40 . border .  center . renderTable . boxTable
              in case maybeDiceVals of
                   Just dv -> formatFn . renderFn $ dv
                   Nothing -> formatFn . renderFn $ []

drawMenu :: TUIState -> Widget Name
drawMenu (TUIState _ _ (Receive _)) =  border $ fill ' '
drawMenu (TUIState _ _ (Request options)) = border . txtWrap $ printOptions options

handleEvent :: BrickEvent Name BEvent -> EventM Name TUIState ()
handleEvent e = case e of
    VtyEvent (V.EvKey V.KEsc []) -> halt
    AppEvent (Receive gvc) -> assign #gameStateViewC gvc >> assign #lastEvent (Receive gvc)
    AppEvent (Request opts) -> undefined
    _ -> pure ()

-- handleRequest :: CantStopOptions -> _
-- handleRequest opts = 


boxTable :: [[Widget n]] -> Table n
boxTable = rowBorders False. columnBorders False . surroundingBorder False
        . setDefaultRowAlignment AlignBottom . setDefaultColAlignment AlignCenter
        . table
square :: Widget Name
square = str "\x02588"

drawPiece :: CantStopResource -> Widget Name
drawPiece (PlayerMarker p) = padTop (Pad 1) . withAttr (playerToColor p) $ square
drawPiece TemporaryMarker = padTop (Pad 1) . withAttr tempMarkAttr $ square

drawSlot :: CSView -> CantStopLocation -> Widget Name
drawSlot gsv t@(TrackSpot _ _) = let
    pieces = listAllShape <$> (gsv ^. #objectsViewC . #locationsViewC . ftAt t)
      in case pieces of
        Nothing -> padTop (Pad 1) square
        (Just []) -> padTop (Pad 1) square
        Just pieces' -> if null pieces'
                        then padTop (Pad 1) square
                        else
                            hBox (fmap drawPiece pieces')
drawSlot _ _ = error "can only draw TrackSpot"


drawVerticalTrack :: CSView -> TrackName -> Widget Name
drawVerticalTrack gsv track = formatTrack  $
                            -- case getHintView (T.pack $ show track ++ "Winner") gsv  of
                            --   Just pText -> let p = read (T.unpack pText) :: Player
                            --                 in fmap (const (drawPiece (PlayerMarker p))) <$> spots
                            fmap (drawSlot gsv) <$> spots
                            where
                                formatTrack = padLeftRight 3 .  renderTable . boxTable
                                spots = reverse [[TrackSpot track i] | i <- [HOne .. maxSlot track]]

