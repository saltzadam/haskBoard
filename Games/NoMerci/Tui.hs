module Tui where

import Brick
import Brick.Game.Tui
import Control.Lens
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Game.Location (inventoryItems, peek', NoCounters)
import Game.Options ()
import Game.Player (Player, displayPlayer)
import qualified Graphics.Vty as V
import Helpers
import Objects

type NMTUIState = TUIState NMLocation NoCounters NMResource NMPhaseName NMPlayName

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
  let card = do
          center <- g `viewLocation` CenterOfTableCard
          whatsThere <- peek' center
          extractCard whatsThere
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
  let p = tui ^. #gameStateView . #currentPlayerView
   in padTop (Pad 1) . hLimit 40 . vLimit 15 $
        simpleMenuBody (drawCurrentPlayer p) (drawOptions printPlay) tui

printPlay :: NMPlayName -> Text
printPlay Take = T.pack "Take card"
printPlay Decline = T.pack "Pay chip"

drawPlayers :: NMView -> Widget Name
drawPlayers g = vBox (drawPlayer g <$> g ^. #playersView . to S.toList)
  where
    drawPlayer :: NMView -> Player -> Widget Name
    drawPlayer g p =
      padTop (Pad 1) $
        str (displayPlayer p ++ maybe " " (drawCards . filter isCard . inventoryItems) (viewLocation g (PlayerStuff p)))
          <=> str ("Chips: " ++ maybe "" show (viewHowManyAt g (PlayerStuff p) Chip))

drawCards :: [NMResource] -> String
drawCards xs = surround (unwords . fmap show . mapMaybe extractCard $ xs)
  where
    surround string = if not (null string) then "[" ++ string ++ "]" else string

theAttrMap :: AttrMap
theAttrMap = attrMap (V.white `on` V.black) []
