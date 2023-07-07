{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use head" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Tui
    (app)
    where

import Agent (CSEvent)
import Brick
import Brick.Game.Tui
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Table
import Control.Lens
import Data.Finitary (inhabitants)
import Data.List (sort)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Dice (renderDice)
import Draw
import Game.Helpers
import Game.Location (listAllShape)
import Objects
import Game.Player (mkPlayers)
import qualified Data.Map as M
import qualified Data.Set as S

type CSTUIState = TUIState CantStopLocation CantStopCounterName CantStopResource CantStopPhaseName CantStopPlayName CantStopIssue

app :: App CSTUIState CSEvent Name
app =
  App
    { appDraw = drawUIView,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return (),
      appAttrMap = const theAttrMap
    }

drawUIView :: CSTUIState -> [Widget Name]
drawUIView tui =
  let g = tui ^. #gameStateView
   in [hLimit 85 $ drawBoardView g <+> (drawDice g <=> drawMenu tui)] -- [ drawBoard g <+> (drawDice g <=> drawMenu g)]

drawBoardView :: CSView -> Widget Name
drawBoardView gsv = border $ padBottom Max . padTop Max . renderTable . boxTable $ widgets
  where
    widgets = [drawVerticalTrack gsv <$> [Two .. Twelve], str . showNum <$> [2 .. 12]]
    showNum i = show i ++ "    "

drawDice :: CSView -> Widget Name
drawDice csv =
  let maybeDiceVals' = fmap (viewCounterVal csv) (inhabitants :: [CantStopCounterName])
      maybeDiceVals = [[maybeDiceVals' !! 0, maybeDiceVals' !! 1], [maybeDiceVals' !! 2, maybeDiceVals' !! 3]]
      renderFn = fmap (padLeftRight 1 . str . renderDice)
      formatFn = vLimitPercent 40 . border . center . renderTable . boxTable
   in formatFn . fmap renderFn $ maybeDiceVals

drawMenu :: CSTUIState -> Widget Name
drawMenu tui =
  let p = tui ^. #gameStateView . #currPlayer
      playerW = withAttr (playerToColor p) (txtWrap (T.pack . show $ p))
   in border . padBottom Max $
        case tui ^. #tuiMode of
          Ask options -> playerW <=> txtWrap (printOptions options)
          ShowState -> playerW <=> fill ' '
          EndGame -> strWrap ("The winner is Player " ++ show (view #num . fromJust $ tui ^. #winner))

boxTable :: [[Widget n]] -> Table n
boxTable =
  rowBorders False
    . columnBorders False
    . surroundingBorder False
    . setDefaultRowAlignment AlignBottom
    . setDefaultColAlignment AlignCenter
    . table

square :: Widget Name
square = str "\x02588"

emptySpace :: Int -> Widget n
emptySpace i = str (replicate i ' ')

drawPiece :: CantStopResource -> Widget Name
drawPiece (PlayerMarker p) = padTop (Pad 1) . withAttr (playerToColor p) $ square
drawPiece TemporaryMarker = padTop (Pad 1) . withAttr tempMarkAttr $ square

-- drawSlot :: CSView -> CantStopLocation -> Widget Name
-- drawSlot csv t@(TrackSpot _ _) =
--   let pieces = listAllShape <$> viewLocation csv t
--       numPieces = maybe 0 length pieces
--    in case pieces of
--         Nothing -> padTop (Pad 1) (square <+> emptySpace 4)
--         (Just []) -> padTop (Pad 1) (square <+> emptySpace 4)
--         Just pieces' -> hBox (fmap drawPiece (sort pieces') ++ [emptySpace (4 - numPieces)])
-- drawSlot _ _ = error "can only draw TrackSpot"

drawVerticalTrack :: CSView -> TrackName -> Widget Name
drawVerticalTrack csv trackName = let
      vals = M.fromSet (viewCounterVal csv . (`PlayerTrack` trackName)) (S.fromList (mkPlayers 4))
    in undefined
  where
    formatTrack = renderTable . boxTable
    -- spots = reverse [[TrackSpot track i] | i <- [HOne .. maxSlot track]]
