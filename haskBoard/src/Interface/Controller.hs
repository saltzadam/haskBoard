{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Interface.Controller
  ( PlayerInterface (..),
    GameController (..),
    agentToInterface,
    controllerInterface,
    buildInterface,
  )
where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Exception (Exception, throwIO)
import Control.Lens (at, makeLenses, to, (^.))
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text
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

data ControllerException = NoSuchInterface Player deriving (Eq, Ord, Show)

instance Exception ControllerException

-- | An 'Interface' that talks to each player through their channels.
controllerInterface :: GameController l cn r ph pl -> Interface l cn r ph pl
controllerInterface gc =
  Interface
    { choose = sendChoice gc,
      update = sendUpdate gc,
      announceWinners = sendWinners gc,
      announce = sendAnnouncement gc
    }

sendUpdate :: GameController l cn r ph pl -> GameState l cn r ph pl -> Map Player Int -> IO ()
sendUpdate gc gs scores = traverse_ send (gc ^. #playerInterfaces . to M.toList)
  where
    send (p, interface) =
      writeChan (interface ^. #fromGameChannel) (SendState (viewGameStateAs gs (LookAs p)) scores)

sendChoice :: GameController l cn r ph pl -> GameState l cn r ph pl -> Options pl -> IO pl
sendChoice gc gs opts = case gc ^. #playerInterfaces . at chooser of
  Nothing -> throwIO (NoSuchInterface chooser)
  Just interface -> do
    writeChan (interface ^. #fromGameChannel) (SendOptions (viewGameStateAs gs (LookAs chooser)) opts)
    readChan (interface ^. #toGameChannel)
  where
    chooser = opts ^. #owner

sendWinners :: GameController l cn r ph pl -> [Player] -> IO ()
sendWinners gc winners =
  traverse_ (\i -> writeChan (i ^. #fromGameChannel) (SendWinners winners)) (gc ^. #playerInterfaces . to M.elems)

sendAnnouncement :: GameController l cn r ph pl -> Maybe Player -> Text -> IO ()
sendAnnouncement gc speaker announcement =
  traverse_ (\i -> writeChan (i ^. #fromGameChannel) (SendAnnouncement speaker announcement)) (gc ^. #playerInterfaces . to M.elems)

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

