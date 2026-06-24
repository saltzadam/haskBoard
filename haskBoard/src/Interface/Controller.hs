{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Interface.Controller
  ( PlayerInterface (..),
    GameController (..),
    agentToInterface,
    chooseInterface,
    buildInterface,
  )
where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Exception (Exception)
import Control.Exception.Base (throw)
import Control.Lens (at, makeLenses, to, (^.))
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import GHC.Generics (Generic)
import Game.Agent (Agent (..))
import Game.Choose
import Game.GameState (GameState)
import Game.Options
import Game.Player (Player)
import Game.View
import Game.Visibility (LookerType (..))

-- Defines interaces and controllers.

-- A player consists of two channels: one to send info, the other to get plays.
data PlayerInterface l cn r ph pl = PlayerInterface
  { fromGameChannel :: Chan (GameToInterfacePayload l cn r ph pl),
    toGameChannel :: Chan pl
  }
  deriving (Generic)

-- A game controller is just a mapping from players to interfaces.
newtype GameController l cn r ph pl = GameController
  { playerInterfaces :: Map Player (PlayerInterface l cn r ph pl)
  }
  deriving (Generic)

makeLenses ''PlayerInterface
makeLenses ''GameController

-- Interpret the Interface effect using a controller
chooseInterface ::
  (IOE :> es) =>
  GameController l cn r ph pl ->
  Eff (Interface l cn r ph pl : es) a ->
  Eff es a
chooseInterface controller = interpret $ \_ -> \case
  Update gs scores -> sendUpdate controller gs scores
  Choose gs opts -> sendChoice controller gs opts
  AnnounceWinners winners -> sendWinners controller winners
  Announce speaker announcement -> sendAnnouncement controller speaker announcement

data ControllerException = NoSuchInterface Player deriving (Eq, Ord, Show)

instance Exception ControllerException

-- sendUpdate :: _
sendUpdate :: (IOE :> es) => GameController l cn r ph pl -> GameState l cn r ph pl -> Map Player Int -> Eff es ()
sendUpdate gc gs scores = liftIO $ traverse_ (sendUpdate' gs) (gc ^. #playerInterfaces . to M.toList)
  where
    sendUpdate' :: GameState l cn r ph pl -> (Player, PlayerInterface l cn r ph pl) -> IO ()
    sendUpdate' gs' (p, interface) =
      writeChan
        (interface ^. #fromGameChannel)
        (SendState (viewGameStateAs gs' (LookAs p)) scores)

-- TODO: use some other idiom w/ throw
sendChoice :: forall l cn r ph pl es. (IOE :> es) => GameController l cn r ph pl -> GameState l cn r ph pl -> Options pl -> Eff es pl
sendChoice gc@(GameController interfaceMap) gs opts =
  if chooser `notElem` M.keys interfaceMap
    then throw (NoSuchInterface chooser)
    else sendChoice' gc gs opts
  where
    chooser = opts ^. #owner

sendChoice' :: forall l cn r ph pl es. (IOE :> es) => GameController l cn r ph pl -> GameState l cn r ph pl -> Options pl -> Eff es pl
sendChoice' gc gs opts = case gc ^. #playerInterfaces . at chooser of
  Nothing -> throw (NoSuchInterface chooser)
  Just interface -> liftIO $ do
    let gsv = viewGameStateAs gs (LookAs chooser)
    writeChan (interface ^. #fromGameChannel) (SendOptions gsv opts)
    readChan (interface ^. #toGameChannel)
  where
    chooser = opts ^. #owner

sendWinners :: (IOE :> es) => GameController l cn r ph pl -> [Player] -> Eff es ()
sendWinners gc winners = liftIO $ traverse_ sendWinners' (gc ^. #playerInterfaces . to M.elems)
  where
    sendWinners' :: PlayerInterface l cn r ph pl -> IO ()
    sendWinners' interface = writeChan (interface ^. #fromGameChannel) (SendWinners winners)

sendAnnouncement :: (IOE :> es) => GameController l cn r ph pl -> Maybe Player -> Text -> Eff es ()
sendAnnouncement gc speaker announcement = liftIO $ traverse_ sendAnnouncement' (gc ^. #playerInterfaces . to M.elems)
  where
    sendAnnouncement' :: PlayerInterface l cn r ph pl -> IO ()
    sendAnnouncement' interface = writeChan (interface ^. #fromGameChannel) (SendAnnouncement speaker announcement)

buildInterface :: [Player] -> IO (GameController l cn r ph pl)
buildInterface ps = GameController . M.fromList <$> traverse go ps
  where
    go :: Player -> IO (Player, PlayerInterface l cn r ph pl)
    go p = do
      c0 <- newChan :: IO (Chan (GameToInterfacePayload l cn r ph pl))
      c1 <- newChan :: IO (Chan pl)
      return (p, PlayerInterface c0 c1)

agentToInterface :: Agent l cn r ph pl IO -> PlayerInterface l cn r ph pl
agentToInterface agent = PlayerInterface (agent ^. #fromGameChannel) (agent ^. #toGameChannel)

