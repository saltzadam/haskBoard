{-# LANGUAGE LambdaCase #-}

module Interface.Choose where

import Control.Concurrent (Chan, readChan, writeChan)
import Data.Finitary (Finitary)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Game.Choose
import Game.View
import Game.Visibility (LookerType)

-- A chooser.

chooseChan ::
  (IOE :> es, Show pl, Finitary l, Finitary cn, Show l, Show r, Show cn, Show ph) =>
  LookerType -> -- could be a list of clients or something in the future
  Chan (GameToInterfacePayload l cn r ph pl) ->
  Chan pl ->
  Eff (Interface l cn r ph pl : es) a ->
  Eff es a
chooseChan viewer gameToClientChan clientToGameChan = interpret $ \_ -> \case
  Update gs scores -> liftIO $ writeChan gameToClientChan (SendState (viewGameStateAs gs viewer) scores)
  Choose gsv options -> liftIO $ do
    writeChan gameToClientChan (SendOptions (viewGameStateAs gsv viewer) options)
    readChan clientToGameChan
  AnnounceWinners winners -> liftIO (writeChan gameToClientChan (SendWinners winners))
  Announce speaker announcement -> liftIO (writeChan gameToClientChan (SendAnnouncement speaker announcement))
