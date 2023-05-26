{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Tui
    where

import Brick (App(..), BrickEvent(..), neverShowCursor, EventM, AttrMap, attrMap, on, AttrName, withAttr, str, attrName, hBox, Padding (..), padLeftRight, (<+>), vLimitPercent, (<=>), fill, halt, put, txt, txtWrap, padBottom)
import Brick.Types (Widget)
import Control.Lens ((^.), view, assign, use, to)
import Game.Player (Player (..))
import qualified Graphics.Vty as V
import Game.Location (listAllShape)
import Brick.Widgets.Table (table, Table, rowBorders, columnBorders, surroundingBorder, renderTable, ColumnAlignment (..), setDefaultColAlignment, RowAlignment (..), setDefaultRowAlignment)
import Brick.Widgets.Core (padTop)
import Brick.Widgets.Border
import FinitaryMap ((!!!),ftAt)
import Data.Finitary (inhabitants)
import Dice (renderDice)
import Brick.Widgets.Center
import Objects
import Game.Helpers (viewCounterVal)
import GHC.Generics (Generic)
import Draw
import Control.Lens.TH (makeFields)
import Game.View (viewGameStateAs')
import Brick.BChan (BChan, writeBChan)
import Safe (readMay)
import Control.Monad.Trans (liftIO)
import qualified Data.Foldable as F
import Game.GameState (GameState)


type Name = ()

data BEvent = Receive CSView
            | Request CantStopOptions
            | Answer PlayName
            deriving (Generic)

instance Show BEvent where
    show (Receive g) = "Receive"
    show (Request opts) = "Request (" ++ show opts ++ ")"
    show (Answer play) = "Answer (" ++ show play ++ ")"

data TUIMode options = ShowState | Ask options | EndGame


data TUIState = TUIState { gameStateView :: CSView
                        , viewer :: Player
                        , lastEvent :: Maybe BEvent
                        , tuiMode :: TUIMode CantStopOptions
                        , brickToGameChan :: BChan PlayName
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
drawUIView tui = let g = tui ^. #gameStateView in 
                     [drawBoardView g <+> (drawDice g  <=> drawMenu tui)] -- [ drawBoard g <+> (drawDice g <=> drawMenu g)]

drawBoardView :: CSView -> Widget Name
drawBoardView gsv = border $ padTop Max . renderTable . boxTable $ [drawVerticalTrack gsv <$> [Two .. Twelve], str . show <$> [2 .. 12] ]


drawDice :: CSView -> Widget Name
drawDice csv = let
    maybeDice = fmap (\i -> csv ^. #objectsView . #countersView . ftAt i)  (inhabitants :: [CantStopCounterName])
    maybeDiceVals' = fmap (view #val) <$> maybeDice
    maybeDiceVals = [[maybeDiceVals' !! 0, maybeDiceVals' !! 1], [maybeDiceVals' !! 2, maybeDiceVals' !! 3]]
    renderFn =  fmap (padLeftRight 1 . str . renderDice) 
    formatFn =  vLimitPercent 40 . border .  center . renderTable . boxTable
                in formatFn . fmap renderFn $ maybeDiceVals

drawMenu :: TUIState -> Widget Name
drawMenu tui = case tui ^. #tuiMode of
                 Ask options -> border . txtWrap $ printOptions options
                 ShowState -> border . fill $ ' '
                 EndGame -> border . txtWrap $ "game over!"
                 

handleEvent :: BrickEvent Name BEvent -> EventM Name TUIState ()
handleEvent e =  do
    mode <- use #tuiMode
    case mode of
      EndGame -> case e of 
          VtyEvent (V.EvKey V.KEsc []) -> halt
          _ -> return ()
      ShowState -> case e of 
          VtyEvent (V.EvKey V.KEsc []) -> halt
          AppEvent (Receive gsv) -> do
              assign #gameStateView gsv
              assign #lastEvent (Just (Receive gsv))
              assign #tuiMode ShowState
          AppEvent (Request opts) ->  do
              assign #lastEvent (Just (Request opts))
              assign #tuiMode (Ask opts)
          _ -> return ()
      Ask options -> case e of
          VtyEvent (V.EvKey V.KEsc []) -> halt
          VtyEvent (V.EvKey (V.KChar c) []) -> 
              case (readMay [c] :: Maybe Int) of
                Nothing -> pure ()
                Just i -> case options ^. #legal . to (safeIndexList (i-1)) of
                            Nothing -> pure ()
                            Just opt -> do
                                chan <- use #brickToGameChan
                                liftIO $ writeBChan chan opt
          _ -> return ()

 

safeIndexList :: Foldable f => Int -> f a -> Maybe a
safeIndexList i xs = if i < 0 then Nothing else safeIndexList' i (F.toList xs)
                         where
                             safeIndexList' i (x:xs) = if i == 0 then Just x else safeIndexList' (i-1) xs
                             safeIndexList' _ [] = Nothing


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
drawSlot csv t@(TrackSpot _ _) = let
    pieces = listAllShape <$> (csv ^. #objectsView . #locationsView . ftAt t)
      in case pieces of
        Nothing -> padTop (Pad 1) square
        (Just []) -> padTop (Pad 1) square
        Just pieces' -> if null pieces'
                        then padTop (Pad 1) square
                        else
                            hBox (fmap drawPiece pieces')
drawSlot _ _ = error "can only draw TrackSpot"


drawVerticalTrack :: CSView -> TrackName -> Widget Name
drawVerticalTrack csv track = formatTrack  $
                            -- case getHintView (T.pack $ show track ++ "Winner") gsv  of
                            --   Just pText -> let p = read (T.unpack pText) :: Player
                            --                 in fmap (const (drawPiece (PlayerMarker p))) <$> spots
                            fmap (drawSlot csv) <$> spots
                            where
                                formatTrack = padLeftRight 3 .  renderTable . boxTable
                                spots = reverse [[TrackSpot track i] | i <- [HOne .. maxSlot track]]

