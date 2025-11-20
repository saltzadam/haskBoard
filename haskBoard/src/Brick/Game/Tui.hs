{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Brick.Game.Tui where

import Brick
import Brick.BChan
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Effectful (MonadIO (..))
import GHC.Generics (Generic)
import Game.Agent (BEvent (..), extractReceive)
import Game.Options (Options)
import Game.Player (Player)
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
        assign #gameStateView gsv
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
        -- assign #lastEvent (Just (Request opts))
        -- in batch, just read first item
        doBatch <- use #batchUpdates
        when
          doBatch
          ( do
              queue <- use #eventQueue
              let rQueue = mapMaybe extractReceive queue
              let endState = listToMaybe rQueue
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

simpleHandler :: TUIEventHandler name l cn r ph pl
simpleHandler = basicHandler <> simpleOptionKeyHandler
