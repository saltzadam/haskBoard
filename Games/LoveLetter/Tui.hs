module Tui where

import Agent
import Brick
import Brick.Game.Tui
import Brick.Widgets.Border (border)
import Brick.Widgets.TextStream
import Control.Lens ((^.))
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import FinitaryMap (ftAt)
import GHC.Generics (Generic)
import Game.Location (inventory)
import Game.Player (Player, displayPlayer)
import Graphics.Vty (defAttr)
import Objects
import Text

data Name = LogView | PlayerView | InstructionsView deriving (Eq, Ord, Show, Generic)

type LLTUIState = TUIState LLLocation Void LLResource LLPhaseName LLPlayName LLIssue

app :: App LLTUIState LLEvent Name
app =
  App
    { appDraw = drawUIView,
      appChooseCursor = neverShowCursor,
      appHandleEvent = runHandler simpleHandler,
      appStartEvent = return (),
      appAttrMap = const $ attrMap defAttr []
    }

drawUIView :: TUIState LLLocation LLCounters LLResource LLPhaseName LLPlayName LLIssue -> [Widget Name]
drawUIView tui@(TUIState {gameStateView = gsv, announcements = announcements}) =
  [ hBox [vBox [playerView gsv, choiceView tui], logView announcements] -- <+> instructionsView
  ]


choiceView tui =
  (border . hLimit 30) $
    let p = tui ^. #gameStateView . #currentPlayerView
     in simpleMenuBody (coloredPlayerWidget p) (drawOptions (T.pack . show)) tui

logView :: [(Maybe Player, T.Text)] -> Widget Name
logView = border . textStream LogView 40 20 . fmap snd

instructionsView :: Widget Name
instructionsView = hLimit 30 . vLimit 50 $ border (txt instructions)

mapToList :: Map a Int -> [a]
mapToList = concatMap (\(a, i) -> replicate i a) . M.toList

playerView :: LLView -> Widget Name
playerView llview =
  let players = S.toList (llview ^. #playersView)
      getHand player = (llview ^. #objectsView . #locationsView . ftAt (Hand player))
      printHand' player = intercalate ", " . fmap show . mapMaybe extractChar . mapToList . inventory <$> getHand player
      printHand player = fromMaybe "" (printHand' player)
      maxPlayerLength = maximum (fmap (textWidth . displayPlayer) players)
      printPlayer player =
        let playerLength = textWidth . displayPlayer $ player
         in T.pack (displayPlayer player <> replicate (2 + (maxPlayerLength - playerLength)) ' ' <> printHand player)
   in border . hLimit 30 $ txtWrap (T.unlines (printPlayer <$> players))

-- theAttrMap = undefined
