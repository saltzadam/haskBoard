{-# LANGUAGE LambdaCase #-}

module Interface.Choose where

import Control.Concurrent (Chan, readChan, writeChan)
import Data.Finitary (Finitary)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import GHC.Generics (Generic)
import Game.Choose
import Game.Options (Options)
import Game.Player
import Game.View
import Game.Visibility (LookerType)

data GameToInterfacePayload l cn r ph pl i
  = SendState (GameStateView l cn r ph)
  | SendOptions (GameStateView l cn r ph) (Options pl i)
  | SendWinners [Player]
  deriving (Generic)

chooseChan ::
  (IOE :> es, Show pl, Show i, Finitary l, Finitary cn, Show l, Show r, Show cn, Show ph) =>
  LookerType -> -- could be a list of clients or something in the future
  Chan (GameToInterfacePayload l cn r ph pl i) ->
  Chan pl ->
  Eff (Interface l cn r ph pl i : es) a ->
  Eff es a
chooseChan viewer gameToClientChan clientToGameChan = interpret $ \_ -> \case
  Update gs -> liftIO $ writeChan gameToClientChan (SendState (viewGameStateAs gs viewer))
  Choose gsv options -> liftIO $ do
    writeChan gameToClientChan (SendOptions (viewGameStateAs gsv viewer) options)
    readChan clientToGameChan
  AnnounceWinners winners -> liftIO (writeChan gameToClientChan (SendWinners winners))
