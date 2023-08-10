module Tui where

import Brick (Widget, App (..), (<=>), (<+>), txt)
import Control.Lens ((^.))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import FinitaryMap (ftAt)
import Game.Location (inventory)
import Game.View (GameStateView (..))
import Objects 
import Brick.Game.Tui (TUIState (..), handleEvent)
import Agent
import Brick.Main
import Data.Void (Void)
import Data.Foldable (find)

icons :: Character -> Char
icons Princess = '👸'
icons Countess = '💃'
icons King = '👑'
icons Prince = '🤴'
icons Priest = '🧙'
icons Baron = '👲'
icons Handmaid = '👩'
icons Guard = '💂'

type Name = ()

type LLTUIState = TUIState LLLocation Void LLResource LLPhaseName LLPlayName LLIssue

app :: App LLTUIState LLEvent Name
app =
  App
    { appDraw = drawUIView,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return (),
      appAttrMap = const theAttrMap
    }

drawUIView TUIState{gameStateView = gsv, eventQueue = eventQueue} = [
    (playerView gsv <=> logView eventQueue) <+> instructionsView
                                           ]
logView stream = announce (find isAnnounceable stream) where
    announce = undefined
    isAnnounceable 

mapToList :: Map a Int -> [a]
mapToList = concatMap (\(a, i) -> replicate i a) . M.toList

playerView :: LLView -> Widget Name
playerView llview =
  let players = S.toList (llview ^. #playersView)
      getHand player = (llview ^. #objectsView . #locationsView . ftAt (Hand player))
      mapResource (Card character) = Just (icons character)
      mapResource _ = Nothing
      printHand' player = mapMaybe mapResource . mapToList . inventory <$> getHand player
      printHand player = fromMaybe "" (printHand' player)
      printPlayer player = T.pack (show player <> " " <> printHand player)
   in txt (T.unlines (printPlayer <$> players))

theAttrMap = undefined
