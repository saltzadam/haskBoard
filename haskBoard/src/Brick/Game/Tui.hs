{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Brick.Game.Tui where

import Brick
import Brick.BChan
import Brick.Widgets.Table
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful (MonadIO (..))
import GHC.Generics (Generic)
import Game.Agent (BEvent (..), extractReceive, extractRequest)
import Game.Options (Options)
import Game.Player (Player (..), displayPlayer, displayPlayerT)
import Game.View (GameStateView)
import qualified Graphics.Vty as V
import Safe (readMay)
import Util

data TUIMode options = ShowState | Ask options | EndGame deriving (Eq, Ord, Show)

data TUIState l cn r ph pl = TUIState
  { gameStateView :: GameStateView l cn r ph,
    viewer :: Player,
    tuiMode :: TUIMode (Options pl),
    eventQueue :: [BEvent l cn r ph pl],
    brickToGameChan :: BChan pl,
    winner :: Maybe Player,
    batchUpdates :: Bool,
    announcements :: [(Maybe Player, Text)]
  }
  deriving (Generic)

makeFields 'TUIState

newtype TUIEventHandler name l cn r ph pl = TUIEventHandler (BrickEvent name (BEvent l cn r ph pl) -> EventM name (TUIState l cn r ph pl) (Maybe ())) deriving (Generic)

instance Semigroup (TUIEventHandler name l cn r ph pl) where
  (<>) (TUIEventHandler f) (TUIEventHandler g) = TUIEventHandler (\x -> liftA2 (<|>) (f x) (g x))

instance Monoid (TUIEventHandler name l cn r ph pl) where
  mempty = TUIEventHandler (const (return Nothing))

runHandler :: TUIEventHandler name l cn r ph pl -> BrickEvent name (BEvent l cn r ph pl) -> EventM name (TUIState l cn r ph pl) ()
runHandler (TUIEventHandler f) event = fromMaybe () <$> f event

escHandler :: TUIEventHandler name l cn r ph pl
escHandler = TUIEventHandler escHandler'
  where
    escHandler' (VtyEvent (V.EvKey V.KEsc [])) = Just <$> halt
    escHandler' _ = return Nothing

receiveHandler :: TUIEventHandler name l cn r ph pl
receiveHandler = TUIEventHandler receiveHandler'
  where
    receiveHandler' (AppEvent (Receive gsv)) =
      Just <$> do
        mode <- use #tuiMode
        case mode of
          EndGame -> modifying #eventQueue (Receive gsv :)
          _ -> do
            doBatch <- use #batchUpdates
            -- in batch, add items to front
            if doBatch
              then modifying #eventQueue (Receive gsv :)
              else assign #gameStateView gsv
            assign #tuiMode ShowState
    receiveHandler' _ = return Nothing

requestHandler :: TUIEventHandler name l cn r ph pl
requestHandler = TUIEventHandler requestHandler'
  where
    requestHandler' (AppEvent (Request opts)) =
      Just <$> do
        mode <- use #tuiMode
        case mode of
          EndGame -> modifying #eventQueue (Request opts :)
          _ -> do
            -- in batch, just read first item
            doBatch <- use #batchUpdates
            when
              doBatch
              ( do
                  queue <- use #eventQueue
                  let endState = listToMaybe (mapMaybe extractReceive queue)
                  maybe (return ()) (assign #gameStateView) endState
                  assign #eventQueue []
              )
            assign #tuiMode (Ask opts)
    requestHandler' _ = return Nothing

announceWinnersHandler :: TUIEventHandler name l cn r ph pl
announceWinnersHandler = TUIEventHandler announceWinnersHandler'
  where
    announceWinnersHandler' (AppEvent (AnnounceWinner winners)) =
      Just <$> do
        -- assign #lastEvent (Just (AnnounceWinner winners))
        assign #winner (listToMaybe winners)
        assign #tuiMode EndGame
    announceWinnersHandler' _ = return Nothing

announceEventHandler :: TUIEventHandler name l cn r ph pl
announceEventHandler =
  TUIEventHandler
    ( \case
        AppEvent (AnnounceEvent speaker announcement) ->
          Just <$> modifying #announcements ((speaker, announcement) :)
        _ -> return Nothing
    )

endGameContinueHandler :: TUIEventHandler name l cn r ph pl
endGameContinueHandler = TUIEventHandler $ \case
  VtyEvent (V.EvKey V.KEnter []) -> do
    mode <- use #tuiMode
    case mode of
      EndGame -> Just <$> do
        queue <- use #eventQueue
        let latestState   = listToMaybe (mapMaybe extractReceive queue)
            latestRequest = listToMaybe (mapMaybe extractRequest queue)
        maybe (return ()) (assign #gameStateView) latestState
        assign #eventQueue []
        assign #winner Nothing
        assign #announcements []
        assign #tuiMode $ maybe ShowState Ask latestRequest
      _ -> return Nothing
  VtyEvent (V.EvKey (V.KChar 'q') []) -> do
    mode <- use #tuiMode
    case mode of
      EndGame -> Just <$> halt
      _       -> return Nothing
  _ -> return Nothing

simpleOptionKeyHandler :: TUIEventHandler name l cn r ph pl
simpleOptionKeyHandler = TUIEventHandler $ \case
  VtyEvent (V.EvKey (V.KChar c) []) -> do
    mode <- use #tuiMode
    case mode of
      Ask options ->
        case (readMay [c] :: Maybe Int) of
          Nothing -> return Nothing
          Just i -> case options ^. #legal . to (safeIndexList (i - 1)) of
            Nothing -> return Nothing
            Just opt -> do
              chan <- use #brickToGameChan
              liftIO $ writeBChan chan opt
              Just <$> assign #tuiMode ShowState
      _ -> return Nothing
  _ -> return Nothing

basicHandler :: TUIEventHandler name l cn r ph pl
basicHandler =
  escHandler
    <> receiveHandler
    <> requestHandler
    <> announceWinnersHandler
    <> announceEventHandler
    <> endGameContinueHandler

simpleHandler :: TUIEventHandler name l cn r ph pl
simpleHandler = basicHandler <> simpleOptionKeyHandler

-- | Numbered list of legal plays. Pass a function that renders a single play.
drawOptions :: (pl -> Text) -> Options pl -> Widget n
drawOptions renderPlay opts =
  txtWrap . T.unlines . zipWith renderItem [1 ..] . foldr (:) [] $ opts ^. #legal
  where
    renderItem i play = T.pack (show (i :: Int) ++ ") ") <> renderPlay play

-- | Current player label.
drawCurrentPlayer :: Player -> Widget n
drawCurrentPlayer = str . displayPlayer

-- | Announcement log, most recent first (pass `announcements` from TUIState).
drawAnnouncements :: [(Maybe Player, Text)] -> Widget n
drawAnnouncements [] = emptyWidget
drawAnnouncements as = vBox $ map renderAnn as
  where
    renderAnn (Nothing, msg) = txtWrap msg
    renderAnn (Just p, msg) = txtWrap (displayPlayerT p <> T.pack ": " <> msg)

-- | End-of-game result panel.
drawEndGame :: Maybe Player -> Widget n
drawEndGame Nothing = str "Game over."
drawEndGame (Just p) = strWrap ("The winner is " ++ displayPlayer p ++ ".")

-- | Canonical player attribute names ("player0".."player3"). Games put their
-- chosen colours in their AttrMap using these names as keys.
defaultPlayerAttrs :: [AttrName]
defaultPlayerAttrs = map (\i -> attrName ("player" ++ show i)) [0 .. 3 :: Int]

-- | Map a player to one of the 'defaultPlayerAttrs' (0-based).
playerToColor :: Player -> AttrName
playerToColor (Player pn) = defaultPlayerAttrs !! fromEnum pn

-- | Colored player label using a caller-supplied attr lookup.
coloredPlayerWidget' :: (Player -> AttrName) -> Player -> Widget n
coloredPlayerWidget' toAttr p = withAttr (toAttr p) (txtWrap (T.pack (show p)))

-- | Colored player label using 'defaultPlayerAttrs'.
coloredPlayerWidget :: Player -> Widget n
coloredPlayerWidget = coloredPlayerWidget' playerToColor

-- | Standard Ask/ShowState/EndGame menu body. Wrap with border/padding as needed.
simpleMenuBody ::
  Widget n ->
  (Options pl -> Widget n) ->
  TUIState l cn r ph pl ->
  Widget n
simpleMenuBody playerW optW (TUIState {tuiMode = mode, winner = w}) =
  case mode of
    Ask options -> playerW <=> optW options
    ShowState -> playerW <=> fill ' '
    EndGame -> drawEndGame w

-- | Braille dot encoding of a count (8 dots per full character).
chipsDict :: [(Int, String)]
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
  concat (replicate (i `div` 8) "\x28FF")
    ++ fromMaybe "" (lookup (i `rem` 8) chipsDict)

-- | Brick Table with no internal/surrounding borders, bottom-aligned rows,
-- centre-aligned columns. Suitable for game boards rendered as grids.
boxTable :: [[Widget n]] -> Table n
boxTable =
  rowBorders False
    . columnBorders False
    . surroundingBorder False
    . setDefaultRowAlignment AlignBottom
    . setDefaultColAlignment AlignCenter
    . table
