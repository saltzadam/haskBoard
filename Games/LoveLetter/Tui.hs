module Tui where

import Agent
import Brick
import Brick.Game.Tui (TUIMode (..), TUIState (..), runHandler, simpleHandler)
import Brick.Widgets.Border (border)
import Brick.Widgets.TextStream
import Control.Lens (view, (^.))
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import FinitaryMap (ftAt)
import GHC.Generics (Generic)
import Game.Location (inventory)
import Game.Options (Options (..))
import Game.Player (Player (..), displayPlayer)
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

player1Attr, player2Attr, player3Attr, player4Attr :: AttrName
player1Attr = attrName "player0"
player2Attr = attrName "player1"
player3Attr = attrName "player2"
player4Attr = attrName "player3"

playerAttrs :: [AttrName]
playerAttrs = [player1Attr, player2Attr, player3Attr, player4Attr]

playerToColor :: Player -> AttrName
playerToColor (Player i) = playerAttrs !! (fromEnum i)

printOptions :: LLOptions -> Text
printOptions (Options legal' _ _) =
  let legal = NE.toList legal'
      printEnumeratedPlay :: (Int, LLPlayName) -> Text
      printEnumeratedPlay (i, play) = T.pack (show i ++ ") ") <> (T.pack . show $ play)
   in T.unlines . fmap printEnumeratedPlay $ zip [1 ..] legal

choiceView tui =
  (border . hLimit 30) $
    let p = tui ^. #gameStateView . #currPlayer
        playerW = withAttr (playerToColor p) (txtWrap (T.pack . show $ p))
     in case tui ^. #tuiMode of
          Ask options -> playerW <=> txtWrap (printOptions options)
          ShowState -> playerW <=> fill ' '
          EndGame -> strWrap ("The winner is Player " ++ show (view #num . fromJust $ tui ^. #winner))

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
