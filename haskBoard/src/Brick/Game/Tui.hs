{-# LANGUAGE TemplateHaskell #-}
module Brick.Game.Tui
    where
import Game.View (GameStateView)
import Game.Player (Player)
import Game.Options (Options)
import Game.Agent (BEvent (..), extractReceive)
import Brick.BChan 
import GHC.Generics (Generic)
import Control.Lens 
import Brick 
import Graphics.Vty as V
import Effectful (MonadIO(..))
import Control.Monad
import Data.Maybe (mapMaybe, listToMaybe)
import Util
import Safe (readMay)

type Name = ()

data TUIMode options = ShowState | Ask options | EndGame

data TUIState l cn r ph pl i = TUIState { gameStateView :: GameStateView l cn r ph
                         , viewer :: Player
                         , tuiMode :: TUIMode (Options pl i)
                         , eventQueue :: [BEvent l cn r ph pl i]
                         , brickToGameChan :: BChan pl
                         , winner :: Maybe Player
                         , batchUpdates :: Bool
                         } deriving (Generic)


makeFields 'TUIState



handleEvent :: BrickEvent Name (BEvent l cn r ph pl i) -> EventM Name (TUIState l cn r ph pl i) ()
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



