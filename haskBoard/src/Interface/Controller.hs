{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Interface.Controller
  ( PlayerInterface (..),
    GameController (..),
    commonInterface,
    agentToInterface,
    chooseInterface,
    agentFromInterface,
  )
where

import Control.Concurrent (Chan, readChan, writeChan)
import Control.Exception (Exception)
import Control.Exception.Base (throw)
import Control.Lens (at, makeLenses, to, (^.))
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as M
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

data PlayerInterface l cn r ph pl i = PlayerInterface
  { fromGameChannel :: Chan (GameToInterfacePayload l cn r ph pl i),
    toGameChannel :: Chan pl
  }
  deriving (Generic)

newtype GameController l cn r ph pl i = GameController
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

data ControllerException = NoSuchInterface Player deriving (Eq, Ord, Show)

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

-- TODO: use some other idiom w/ throw
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

agentFromInterface ::
  PlayerInterface l cn r ph pl i ->
  (GameStateView l cn r ph -> Options pl i -> m pl) ->
  (GameStateView l cn r ph -> m ()) ->
  ([Player] -> m ()) ->
  Agent l cn r ph pl i m
agentFromInterface (PlayerInterface fromChan toChan) chooser stater winner =
  Agent chooser stater winner fromChan toChan
