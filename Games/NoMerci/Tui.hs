module Tui where

import Agent (NMEvent)
import Brick
import Brick.Game.Tui
import Control.Lens
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Game.Location (inventory, peek')
import Game.Options (Options (..))
import Game.Player (Player, displayPlayer)
import qualified Graphics.Vty as V
import Helpers
import Objects

type NMTUIState = TUIState NMLocation NMCounters NMResource NMPhaseName NMPlayName NMIssue

type Name = ()

app :: App NMTUIState NMEvent Name
app =
  App
    { appDraw = drawUIView,
      appChooseCursor = neverShowCursor,
      appHandleEvent = runHandler simpleHandler,
      appStartEvent = return (),
      appAttrMap = const theAttrMap
    }

drawUIView :: NMTUIState -> [Widget Name]
drawUIView tui =
  let g = tui ^. #gameStateView
   in [hLimit 85 $ (drawBoardView g <=> drawMenu tui) <+> drawPlayers g]

drawBoardView :: NMView -> Widget Name
drawBoardView g =
  let card = (g `viewLocation` CenterOfTableCard) >>= peek' >>= extractCard
      chips = viewHowManyAt g ChipPile Chip
   in fromMaybe drawNothing (drawCard <$> card <*> chips)

drawNothing :: Widget Name
drawNothing = str $ unlines (replicate 9 (replicate 9 ' '))

drawCard :: Int -> Int -> Widget Name
drawCard i chips =
  str $
    unlines
      [ " ------- ",
        "|       |",
        "|       |",
        "|       |",
        cardLine i,
        "|       |",
        chipsLine chips,
        "|       |",
        " ------- "
      ]
  where
    showNum i = if i < 10 then " " ++ show i else show i
    cardLine :: Int -> String
    cardLine i = "|   " ++ showNum i ++ "  |"
    chipsLine :: Int -> String
    chipsLine chips =
      let lenChips = length (drawChips chips)
          leftSpace = (7 - lenChips) `div` 2 + (7 - lenChips) `mod` 2
          rightSpace = (7 - lenChips) `div` 2
       in "|"
            ++ replicate leftSpace ' '
            ++ drawChips chips
            ++ replicate rightSpace ' '
            ++ "|"

drawMenu :: NMTUIState -> Widget Name
drawMenu tui =
  let p = tui ^. #gameStateView . #currPlayer
      playerW = strWrap (displayPlayer p)
   in padTop (Pad 1) . hLimit 15 . vLimit 15 $
        case tui ^. #tuiMode of
          Ask options -> playerW <=> txtWrap (printOptions options)
          ShowState -> playerW <=> fill ' '
          EndGame -> strWrap $ ("The winner is Player " ++ show (view #num . fromJust $ tui ^. #winner))

chipsDict =
  [ (0, ""),
    (1, "\x2840"),
    (2, "\x28C0"),
    (3, "\x28C4"),
    (4, "\x28E4"),
    (5, "\x28E6"),
    (6, "\x28F6"),
    (7, "\x28F7")
  ]

drawChips :: Int -> String
drawChips i =
  let full = i `div` 8
      remainder = i `rem` 8
   in concat (replicate full "\x28FF")
        ++ fromMaybe "" (lookup remainder chipsDict)

printOptions :: NMOptions -> Text
printOptions (Options legal' _ _) =
  let legal = NE.toList legal'
      printEnumeratedPlay :: (Int, NMPlayName) -> Text
      printEnumeratedPlay (i, play) = T.pack (show i ++ ") ") <> printPlay play
   in T.unlines . fmap printEnumeratedPlay $ zip [1 ..] legal

printPlay :: NMPlayName -> Text
printPlay Take = T.pack "Take card"
printPlay Decline = T.pack "Pay chip"

drawPlayers :: NMView -> Widget Name
drawPlayers g = vBox (drawPlayer g <$> g ^. #playersView . to S.toList)
  where
    drawPlayer :: NMView -> Player -> Widget Name
    drawPlayer g p =
      padTop (Pad 1) $
        str (displayPlayer p ++ maybe " " (drawCards . filter isCard . M.keys . inventory) (viewLocation g (PlayerStuff p)))
          <=> str ("Chips: " ++ maybe "" show (viewHowManyAt g (PlayerStuff p) Chip))

drawCards :: [NMResource] -> String
drawCards xs = surround (unwords . fmap show . mapMaybe extractCard $ xs)
  where
    surround string = if not . null $ string then "[" ++ string ++ "]" else string

theAttrMap :: AttrMap
theAttrMap = attrMap (V.white `on` V.black) []
