{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Game.Choose where
import Effectful
import Game.GameState
import Game.Options (Options)
import Effectful.Dispatch.Dynamic
import Control.Concurrent (Chan, writeChan, readChan)
import Game.View (GameStateView, viewGameStateAs)
import Game.Monad (LookerType)
import Data.Finitary (Finitary)
import Game.Player
import GHC.Generics (Generic)

data Interface l cn r ph pl i :: Effect where
  Choose :: GameState l cn r ph pl i -> Options pl i -> (Interface l cn r ph pl i) m pl
  Update :: GameState l cn r ph pl i -> (Interface l cn r ph pl i) m ()
  AnnounceWinners :: [Player] -> (Interface l cn r ph pl i) m ()

type instance DispatchOf (Interface l cn r ph pl i) = 'Dynamic

choose :: (Interface l cn r ph pl i :> es) => GameState l cn r ph pl i -> Options pl i -> Eff es pl
choose gs cs = send (Choose gs cs) 

update :: (Interface l cn r ph pl i :> es) => GameState l cn r ph pl i -> Eff es ()
update gsvc = send (Update gsvc)

announceWinners :: (Interface l cn r ph pl i :> es) => [Player] -> Eff es ()
announceWinners winners = send (AnnounceWinners winners)

data GameToInterfacePayload l cn r ph pl i = SendState (GameStateView l cn r ph)
                                        | SendOptions (GameStateView l cn r ph) (Options pl i)
                                        | SendWinners [Player]
                                        deriving (Generic)

chooseChan :: (IOE :> es, Show pl, Show i,Finitary l, Finitary cn, Show l, Show r, Show cn, Show ph) => LookerType-- could be a list of clients or something in the future
            -> Chan (GameToInterfacePayload l cn r ph pl i)
            -> Chan pl
            -> Eff (Interface l cn r ph pl i : es) a
            -> Eff es a
chooseChan viewer gameToClientChan clientToGameChan = interpret $ \_ -> \case
    Update gs -> liftIO $ writeChan gameToClientChan (SendState (viewGameStateAs gs viewer))
    Choose gsv options -> liftIO $ do
        writeChan gameToClientChan (SendOptions (viewGameStateAs gsv viewer) options)
        readChan clientToGameChan
    AnnounceWinners winners -> liftIO (writeChan gameToClientChan (SendWinners winners))
