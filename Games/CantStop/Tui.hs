{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Tui
    where

import Brick (App(..), BrickEvent(..), neverShowCursor, EventM, AttrMap, attrMap, on, AttrName, withAttr, str, attrName, hBox, Padding (..), padLeftRight, (<+>), vLimitPercent, (<=>), fill, halt, put, txt, txtWrap, padBottom, hLimit,strWrap)
import Data.Maybe ( fromJust, mapMaybe, listToMaybe )
import Brick.Types (Widget)
import Control.Lens ((^.), view, assign, use, to, modifying)
import Game.Player (Player (..))
import qualified Graphics.Vty as V
import Data.List (sort)
import Game.Location (listAllShape, inventory)
import Brick.Widgets.Table (table, Table, rowBorders, columnBorders, surroundingBorder, renderTable, ColumnAlignment (..), setDefaultColAlignment, RowAlignment (..), setDefaultRowAlignment, alignCenter, alignLeft)
import Brick.Widgets.Core (padTop, padLeft, padRight)
import Brick.Widgets.Border
import Data.Finitary (inhabitants)
import Dice (renderDice)
import Brick.Widgets.Center
import Objects
import GHC.Generics (Generic)
import Draw
import Control.Lens.TH (makeFields)
import Brick.BChan (BChan, writeBChan)
import Safe (readMay)
import Control.Monad (unless, when)
import Control.Monad.Trans (liftIO)
import qualified Data.Foldable as F
import qualified Data.Text as T
import Game.Helpers
import Count (notInfinite)
import Game.Agent (BEvent(..))
import Agent (CSEvent)
import Util (safeIndexList)


type Name = ()

-- data CSEvent = Receive CSView
--             | Request CantStopOptions
--             | Answer CantStopPlayName
--             | AnnounceWinner [Player]
--             deriving (Generic)

extractReceive :: CSEvent -> Maybe CSView
extractReceive (Receive gsv) = Just gsv
extractReceive _ = Nothing

data TUIMode options = ShowState | Ask options | EndGame


data TUIState = TUIState { gameStateView :: CSView
                        , viewer :: Player
                        , tuiMode :: TUIMode CantStopOptions
                        , eventQueue :: [CSEvent]
                        , brickToGameChan :: BChan CantStopPlayName
                        , winner :: Maybe Player
                        , batchUpdates :: Bool
                         } deriving (Generic)

makeFields 'TUIState

app :: App TUIState CSEvent Name
app = App {   appDraw = drawUIView
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = const theAttrMap
          }

drawUIView :: TUIState -> [Widget Name]
drawUIView tui = let g = tui ^. #gameStateView in
                     [hLimit 85 $ drawBoardView g <+> (drawDice g  <=> drawMenu tui)] -- [ drawBoard g <+> (drawDice g <=> drawMenu g)]

drawBoardView :: CSView -> Widget Name
drawBoardView gsv = border $ padBottom Max . padTop Max . renderTable .  boxTable $ widgets
    where
        widgets = [drawVerticalTrack gsv <$> [Two .. Twelve], str . showNum <$> [2 .. 12] ]
        showNum i = show i ++ "    "


drawDice :: CSView -> Widget Name
drawDice csv = let
    maybeDiceVals' = fmap (viewCounterVal csv)  (inhabitants :: [CantStopCounterName])
    maybeDiceVals = [[maybeDiceVals' !! 0, maybeDiceVals' !! 1], [maybeDiceVals' !! 2, maybeDiceVals' !! 3]]
    renderFn =  fmap (padLeftRight 1 . str . renderDice)
    formatFn =  vLimitPercent 40 . border .  center . renderTable . boxTable
                in formatFn . fmap renderFn $ maybeDiceVals

drawMenu :: TUIState -> Widget Name
drawMenu tui = let
                         p = tui ^. #gameStateView . #currPlayer
                         playerW = withAttr (playerToColor p) (txtWrap (T.pack . show $ p))
                in border . padBottom Max $
    case tui ^. #tuiMode of
                 Ask options -> playerW <=> txtWrap (printOptions options)
                 ShowState -> playerW <=> fill ' '
                 EndGame -> strWrap $ ("The winner is Player " ++ show (view #num . fromJust $ tui ^. #winner))


handleEvent :: BrickEvent Name CSEvent -> EventM Name TUIState ()
handleEvent e =  do
    mode <- use #tuiMode
    case mode of
      EndGame -> case e of
          VtyEvent (V.EvKey V.KEsc []) -> halt
          _ -> return ()
      _ -> case e of
          VtyEvent (V.EvKey V.KEsc []) -> halt
          AppEvent (Receive gsv) -> do
              assign #gameStateView gsv
              doBatch <- use #batchUpdates
              -- in batch, add items to front
              if doBatch
              then modifying #eventQueue (Receive gsv:)
              else assign #gameStateView gsv
              assign #tuiMode ShowState
          AppEvent (Request opts) ->  do
              -- assign #lastEvent (Just (Request opts))
              -- in batch, just read first item
              doBatch <- use #batchUpdates
              when doBatch (do
                queue <- use #eventQueue
                let rQueue = mapMaybe extractReceive queue
                let endState = listToMaybe rQueue
                maybe (return ()) (assign #gameStateView) endState
                assign #eventQueue [])
              assign #tuiMode (Ask opts)
          AppEvent (AnnounceWinner winners) -> do
              -- assign #lastEvent (Just (AnnounceWinner winners))
              assign #winner (listToMaybe winners)
              assign #tuiMode EndGame
          VtyEvent (V.EvKey (V.KChar c) []) -> do
              case mode of
                Ask options ->
                  case (readMay [c] :: Maybe Int) of
                    Nothing -> pure ()
                    Just i -> case options ^. #legal . to (safeIndexList (i-1)) of
                                Nothing -> pure ()
                                Just opt -> do
                                    chan <- use #brickToGameChan
                                    liftIO $ writeBChan chan opt
                                    assign #tuiMode ShowState
                _ -> return ()
          _ -> return ()




boxTable :: [[Widget n]] -> Table n
boxTable = rowBorders False. columnBorders False . surroundingBorder False
        . setDefaultRowAlignment AlignBottom . setDefaultColAlignment AlignCenter
        . table

square :: Widget Name
square = str "\x02588"

emptySpace :: Int -> Widget n
emptySpace i = str (replicate i ' ')

drawPiece :: CantStopResource -> Widget Name
drawPiece (PlayerMarker p) = padTop (Pad 1) . withAttr (playerToColor p) $ square
drawPiece TemporaryMarker = padTop (Pad 1) . withAttr tempMarkAttr $ square

drawSlot :: CSView -> CantStopLocation -> Widget Name
drawSlot csv t@(TrackSpot _ _) = let
    pieces = listAllShape <$> viewLocation csv t
    numPieces = maybe 0 length pieces
      in case pieces of
        Nothing -> padTop (Pad 1) (square <+> emptySpace 4)
        (Just []) -> padTop (Pad 1) (square <+> emptySpace 4)
        Just pieces' -> hBox (fmap drawPiece (sort pieces') ++ [emptySpace (4 - numPieces)])
drawSlot _ _ = error "can only draw TrackSpot"


drawVerticalTrack :: CSView -> TrackName -> Widget Name
drawVerticalTrack csv track = formatTrack  $
                            fmap (drawSlot csv) <$> spots
                            where
                                formatTrack =   renderTable . boxTable
                                spots = reverse [[TrackSpot track i] | i <- [HOne .. maxSlot track]]

