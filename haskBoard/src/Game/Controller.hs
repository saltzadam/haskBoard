{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Game.Controller
  ( PlayerInterface (..),
    GameController (..),
    chooseInterface,
    commonInterface,
    agentToInterface,
  )
where

import Control.Concurrent (Chan, readChan, writeChan)
import Control.Exception
import Control.Lens (at, makeLenses, to, (^.))
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, listToMaybe)
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import GHC.Generics (Generic)
import Game.Agent (Agent)
import Game.Choose (Interface (..))
import Game.GameState (GameState)
import Game.Monad (LookerType (..))
import Game.Options (Options)
import Game.Player (Player)
import Game.View (GameStateView, viewGameStateAs)
import Interface.Choose (GameToInterfacePayload (..))

-- controller should distribute Interface events and collect results

data PlayerInterface l cn r ph pl i = PlayerInterface
  { fromGameChannel :: Chan (GameToInterfacePayload l cn r ph pl i),
    toGameChannel :: Chan pl
  }
  deriving (Generic)

data GameController l cn r ph pl i = GameController
  { playerInterfaces :: Map Player (PlayerInterface l cn r ph pl i)
  }
  deriving (Generic)

makeLenses ''PlayerInterface
makeLenses ''GameController

chooseInterface ::
  (IOE :> es) =>
  GameController l cn r ph pl i ->
  Eff (Interface l cn r ph pl i : es) a ->
  Eff es a
chooseInterface controller = interpret $ \_ -> \case
  Update gs -> sendUpdate controller gs
  Choose gs opts -> sendChoice controller gs opts
  AnnounceWinners winners -> sendWinners controller winners

data ControllerException = NoSuchInterface Player | NoPlayReturned Player deriving (Eq, Ord, Show)

instance Exception ControllerException

-- sendUpdate :: _
sendUpdate :: (IOE :> es) => GameController l cn r ph pl i -> GameState l cn r ph pl i -> Eff es ()
sendUpdate gc gs = liftIO $ traverse_ (sendUpdate' gs) (gc ^. #playerInterfaces . to M.toList)
  where
    sendUpdate' :: GameState l cn r ph pl i -> (Player, PlayerInterface l cn r ph pl i) -> IO ()
    sendUpdate' gs (p, interface) =
      writeChan
        (interface ^. #fromGameChannel)
        (SendState (viewGameStateAs gs (LookAs p)))

sendChoice :: forall l cn r ph pl i es. (IOE :> es) => GameController l cn r ph pl i -> GameState l cn r ph pl i -> Options pl i -> Eff es pl
sendChoice gc@(GameController interfaceMap) gs opts =
  if chooser `notElem` M.keys interfaceMap
    then throw (NoSuchInterface chooser)
    else sendChoice' gc gs opts
  where
    chooser = opts ^. #owner

sendChoice' :: forall l cn r ph pl i es. (IOE :> es) => GameController l cn r ph pl i -> GameState l cn r ph pl i -> Options pl i -> Eff es pl
sendChoice' gc gs opts = case gc ^. #playerInterfaces . at chooser of
  Nothing -> throw (NoSuchInterface chooser)
  Just interface -> liftIO $ do
    let gsv = viewGameStateAs gs (LookAs chooser)
    writeChan (interface ^. #fromGameChannel) (SendOptions gsv opts)
    readChan (interface ^. #toGameChannel)
  where
    chooser = opts ^. #owner

sendWinners :: (IOE :> es) => GameController l cn r ph pl i -> [Player] -> Eff es ()
sendWinners gc winners = liftIO $ traverse_ (sendWinners' winners) (gc ^. #playerInterfaces . to M.elems)
  where
    sendWinners' :: [Player] -> PlayerInterface l cn r ph pl i -> IO ()
    sendWinners' ps interface = writeChan (interface ^. #fromGameChannel) (SendWinners ps)

commonInterface ::
  [Player] ->
  Chan (GameToInterfacePayload l cn r ph pl i) ->
  Chan pl ->
  GameController l cn r ph pl i
commonInterface ps fromGameChan' toGameChan' =
  let theInterface = PlayerInterface fromGameChan' toGameChan'
   in GameController (M.fromList [(p, theInterface) | p <- ps])

agentToInterface :: Agent l cn r ph pl i IO -> PlayerInterface l cn r ph pl i
agentToInterface agent = PlayerInterface (agent ^. #fromGameChannel) (agent ^. #toGameChannel)
