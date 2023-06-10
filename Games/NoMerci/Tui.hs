{-# LANGUAGE TemplateHaskell #-}
module Tui
    where
import Objects
import Game.Player (Player, displayPlayer)
import GHC.Generics (Generic)
import Brick.BChan (BChan, writeBChan)
import Control.Lens.TH(makeFields)
import Brick (App (..), neverShowCursor, Widget, hLimit, (<+>), (<=>), str, vLimit, strWrap, fill, txtWrap, vBox, BrickEvent (..), EventM, halt, on, padBottom, Padding (..), padTop)
import Control.Lens ((^.), view, to, assign, use, modifying)
import Game.Helpers
import Game.Location (peek, inventory)
import Brick.Widgets.Border (border)
import Data.Text (Text)
import Game.Options (Options(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Maybe (fromJust, mapMaybe, listToMaybe, fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import Agent (NMEvent)
import qualified Graphics.Vty as V
import Game.Agent (BEvent(..), extractReceive)
import Control.Monad (when)
import Safe (readMay)
import Util (safeIndexList)
import Effectful (MonadIO(..))
import Brick.AttrMap (attrMap, AttrMap)
import Data.List (intersperse, delete)
import Count (notInfinite)

type Name = ()

data TUIMode options = ShowState | Ask options | EndGame

data TUIState = TUIState { gameStateView :: NMView
                         , viewer :: Player
                         , tuiMode :: TUIMode NMOptions
                         , eventQueue :: [NMEvent]
                         , brickToGameChan :: BChan NMPlayName
                         , winner :: Maybe Player
                         , batchUpdates :: Bool
                         } deriving (Generic)


makeFields 'TUIState

app :: App TUIState NMEvent Name
app = App {   appDraw = drawUIView
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = const theAttrMap
          }

drawUIView :: TUIState -> [Widget Name]
drawUIView tui = let
    g = tui ^. #gameStateView
    in
        [hLimit 85 $ (drawBoardView g <=> drawMenu tui) <+> drawPlayers g]

drawBoardView :: NMView -> Widget Name
drawBoardView g = let
    card = (g `viewLocation` CenterOfTableCard) >>= peek >>= extractCard
    chips = notInfinite <$> viewHowManyAt g ChipPile Chip
                   in fromMaybe drawNothing (drawCard <$> card <*> chips)
    

drawNothing :: Widget Name
drawNothing = str $ unlines (replicate 9 (replicate 9 ' '))

drawCard :: Int -> Int -> Widget Name
drawCard i chips = str $ unlines [" ------- ",
                            "|       |",
                            "|       |",
                            "|       |",
                           cardLine i,
                            "|       |",
                            chipsLine chips,
                            "|       |",
                            " ------- "]
            where
                showNum i = if i < 10 then " " ++ show i else show i
                cardLine :: Int -> String
                cardLine i = "|   " ++ showNum i ++ "  |"
                chipsLine :: Int -> String
                chipsLine chips = let
                    lenChips = length (drawChips chips)
                    leftSpace = (7 - lenChips) `div` 2 + (7 - lenChips) `mod` 2
                    rightSpace = (7 - lenChips) `div` 2
                                   in "|" 
                                        ++ replicate leftSpace ' ' 
                                        ++ drawChips chips
                                        ++ replicate rightSpace ' '
                                        ++ "|"

drawMenu :: TUIState -> Widget Name
drawMenu tui = let
                p = tui ^. #gameStateView . #currPlayer
                playerW = strWrap (displayPlayer p)
           in padTop (Pad 1) . hLimit 15 . vLimit 15 $
            case tui ^. #tuiMode of
                Ask options -> playerW <=> txtWrap (printOptions options)
                ShowState -> playerW <=> fill ' '
                EndGame -> strWrap $ ("The winner is Player " ++ show (view #num . fromJust $ tui ^. #winner))


chipsDict = [(0, ""), (1,"\x2840"), (2,"\x28C0"), (3,"\x28C4"),
             (4,"\x28E4"), (5,"\x28E6"), (6, "\x28F6"), (7, "\x28F7")]

drawChips :: Int -> String
drawChips i = let
    full = i `div` 8
    remainder = i `rem` 8
        in concat (replicate full "\x28FF")
          ++ fromMaybe "" (lookup remainder chipsDict)


printOptions :: NMOptions -> Text
printOptions (Options legal' _ _) = let
    legal = NE.toList legal'
    printEnumeratedPlay :: (Int,NMPlayName) -> Text
    printEnumeratedPlay (i, play) = T.pack (show i ++ ") ") <> printPlay play
    in
    T.unlines . fmap printEnumeratedPlay $ zip [1..] legal

printPlay :: NMPlayName -> Text
printPlay Take = T.pack "Take card"
printPlay Decline = T.pack "Pay chip"

drawPlayers :: NMView -> Widget Name
drawPlayers g = vBox (drawPlayer g <$> g ^. #playersView . to S.toList)
    where
        drawPlayer :: NMView -> Player -> Widget Name
        drawPlayer g p = padTop (Pad 1) $ str (displayPlayer p)
            <=> str (maybe " " (drawCards . filter isCard . M.keys . inventory) (viewLocation g (PlayerStuff p)))
            <=> str ("Chips: " ++ maybe "" show (viewHowManyAt g (PlayerStuff p) Chip))

drawCards :: [NMResource] -> String
drawCards = unwords . fmap show .  mapMaybe extractCard

handleEvent :: BrickEvent Name NMEvent -> EventM Name TUIState ()
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


theAttrMap :: AttrMap
theAttrMap = attrMap (V.white `on` V.black) []
