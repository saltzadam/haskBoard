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
    { appDraw = renderUIView,
      appChooseCursor = neverShowCursor,
      appHandleEvent = runHandler simpleHandler,
      appStartEvent = return (),
      appAttrMap = const theAttrMap
    }

renderUIView :: NMTUIState -> [Widget Name]
renderUIView tui =
  let g = tui ^. #gameStateView
   in [hLimit 85 $ (renderBoardView g <=> renderMenu tui) <+> renderPlayers g]

renderBoardView :: NMView -> Widget Name
renderBoardView g =
  let card = do
          center <- g `viewLocation` CenterOfTableCard
          whatsThere <- peek' center
          extractCard whatsThere
      chips = viewHowManyAt g ChipPile Chip
   in fromMaybe renderNothing (renderCard <$> card <*> chips)

renderNothing :: Widget Name
renderNothing = str $ unlines (replicate 9 (replicate 9 ' '))

renderCard :: Int -> Int -> Widget Name
renderCard i chips =
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

renderMenu :: NMTUIState -> Widget Name
renderMenu tui =
  let p = tui ^. #gameStateView . #currentPlayerView
   in padTop (Pad 1) . hLimit 40 . vLimit 15 $
        simpleMenuBody (drawCurrentPlayer p) (drawOptions printPlay) tui

printPlay :: NMPlayName -> Text
printPlay Take = T.pack "Take card"
printPlay Decline = T.pack "Pay chip"

renderPlayers :: NMView -> Widget Name
renderPlayers g = vBox (drawPlayer g <$> g ^. #playersView . to S.toList)
  where
    drawPlayer :: NMView -> Player -> Widget Name
    drawPlayer g p =
      padTop (Pad 1) $
        str (displayPlayer p ++ maybe " " (printCards . filter isCard . inventoryItems) (viewLocation g (PlayerStuff p)))
          <=> str ("Chips: " ++ maybe "" show (viewHowManyAt g (PlayerStuff p) Chip))

printCards :: [NMResource] -> String
printCards xs = surround (unwords . fmap show . mapMaybe extractCard $ xs)
  where
    surround string = if not (null string) then "[" ++ string ++ "]" else string

theAttrMap :: AttrMap
theAttrMap = attrMap (V.white `on` V.black) []
