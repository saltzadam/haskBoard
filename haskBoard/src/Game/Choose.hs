{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Game.Choose where
import Effectful
import Game.GameState
import Game.Options (Options)
import Effectful.Crypto.RNG (RNG, randomR)
import qualified Data.List.NonEmpty as NE
import Effectful.Dispatch.Dynamic
import Control.Lens
import Control.Concurrent (Chan, writeChan, readChan)
import Game.View (GameStateView, viewGameStateAs)
import Game.Monad (LookerType)
import Data.Finitary (Finitary)
import Game.Player
import GHC.Generics (Generic)

data Interface l cn r ph pl i :: Effect where
  Choose :: Options pl i -> (Interface l cn r ph pl i) m pl
  Update :: GameState l cn r ph pl i -> (Interface l cn r ph pl i) m ()
  AnnounceWinners :: [Player] -> (Interface l cn r ph pl i) m ()

type instance DispatchOf (Interface l cn r ph pl i) = 'Dynamic

choose :: (Interface l cn r ph pl i :> es) =>  Options pl i -> Eff es pl
choose cs = send (Choose cs) 

update :: (Interface l cn r ph pl i :> es) => GameState l cn r ph pl i -> Eff es ()
update gsvc = send (Update gsvc)

announceWinners :: (Interface l cn r ph pl i :> es) => [Player] -> Eff es ()
announceWinners winners = send (AnnounceWinners winners)

chooseFirst :: Eff (Interface l cn r ph pl i : es) a -> Eff es a
chooseFirst = interpret $ \_ -> \case
  Choose cs -> return (cs ^. #legal . to NE.head)
  Update _ -> return ()
  AnnounceWinners _ -> return ()

chooseRandom :: (RNG :> es) => Eff (Interface l cn r ph pl i : es) a -> Eff es a
chooseRandom = interpret $ \_ -> \case
  Choose cs' ->
    let cs = cs' ^. #legal
        choice = randomR (0, length cs - 1)
     in fmap (cs NE.!!) choice
  Update _ -> return ()
  AnnounceWinners _ -> return ()

data GameToInterfacePayload l cn r ph pl i = SendState (GameStateView l cn r ph)
                                        | SendOptions (Options pl i)
                                        | SendWinners [Player]
                                        deriving (Generic)

chooseChan :: (IOE :> es, Show pl, Show i,Finitary l, Finitary cn, Show l, Show r, Show cn, Show ph) => LookerType-- could be a list of clients or something in the future
            -> Chan (GameToInterfacePayload l cn r ph pl i)
            -> Chan pl
            -> Eff (Interface l cn r ph pl i : es) a
            -> Eff es a
chooseChan viewer gameToClientChan clientToGameChan = interpret $ \_ -> \case
    Update gs -> liftIO $ writeChan gameToClientChan (SendState (viewGameStateAs gs viewer))
    Choose options -> liftIO $ do
        writeChan gameToClientChan (SendOptions options)
        readChan clientToGameChan
    AnnounceWinners winners -> liftIO (writeChan gameToClientChan (SendWinners winners))
