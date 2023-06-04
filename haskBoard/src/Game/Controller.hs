{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
    {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Game.Controller
    (PlayerInterface(..),
    GameController(..),
    chooseInterface,
    commonInterface,
    agentToInterface
    )
    where
import GHC.Generics (Generic)
import Data.Map (Map)
import Game.Player (Player)
import Control.Concurrent (Chan,  writeChan, readChan)
import Game.Choose (GameToInterfacePayload (..), Interface (..))
import Effectful (Eff, liftIO, IOE, (:>))
import Control.Lens (makeLenses, (^.), at, to)
import Effectful.Dispatch.Dynamic (interpret)
import Data.Foldable (traverse_)
import qualified Data.Map as M
import Game.Options (Options)
import Control.Exception
import Data.Maybe (catMaybes)
import Game.View ( viewGameStateAs)
import Game.GameState (GameState)
import Game.Monad (LookerType(..))
import Game.Agent (Agent)

-- controller should distribute Interface events and collect results

data PlayerInterface l cn r ph pl i = PlayerInterface {
    fromGameChannel :: Chan (GameToInterfacePayload l cn r ph pl i),
    toGameChannel :: Chan pl} deriving (Generic)

data GameController l cn r ph pl i = GameController {
    playerInterfaces :: Map Player (PlayerInterface l cn r ph pl i)
                                     } deriving (Generic)

makeLenses ''PlayerInterface
makeLenses ''GameController


chooseInterface :: (IOE :> es) => GameController l cn r ph pl i
    -> Eff (Interface l cn r ph pl i : es) a
    -> Eff es a
chooseInterface controller = interpret $ \_ -> \case
    Update gs ->  sendUpdate controller gs
    -- TODO: too much Maybe junk
    -- TODO: redundant arguments? 
    Choose gs opts ->  sendChoice controller gs opts
    AnnounceWinners winners -> sendWinners controller winners

data ControllerException = NoSuchInterface Player deriving (Eq, Ord, Show)
instance Exception ControllerException

-- sendUpdate :: _
sendUpdate :: (IOE :> es) => GameController l cn r ph pl i -> GameState l cn r ph pl i -> Eff es ()
sendUpdate gc gs = liftIO $ traverse_ (sendUpdate' gs) (gc ^. #playerInterfaces . to M.toList)
    where
        sendUpdate' :: GameState l cn r ph pl i -> (Player, PlayerInterface l cn r ph pl i) -> IO ()
        sendUpdate' gs (p,interface) = writeChan (interface ^. #fromGameChannel)
                                               (SendState (viewGameStateAs gs (LookAs p)))

sendChoice  :: forall l cn r ph pl i es . (IOE :> es) => GameController l cn r ph pl i -> GameState l cn r ph pl i -> Options pl i ->  Eff es pl
sendChoice gc@(GameController interfaceMap) gs opts = let chooser = opts ^. #owner
    in
        if chooser `notElem` M.keys interfaceMap
        then throw (NoSuchInterface chooser)
        else
            head . catMaybes <$> traverse (sendChoice' gc gs opts) (M.keys interfaceMap)

sendChoice' :: forall l cn r ph pl i es . (IOE :> es) => GameController l cn r ph pl i -> GameState l cn r ph pl i -> Options pl i -> Player -> Eff es (Maybe pl)
sendChoice' gc gs opts p =  if p == chooser
                        then case gc ^. #playerInterfaces . at p of
                              Nothing -> throw (NoSuchInterface chooser)
                              Just interface -> liftIO $ do
                                  let gsv = viewGameStateAs  gs (LookAs p)
                                  writeChan (interface ^. #fromGameChannel) (SendOptions gsv opts)
                                  Just <$> readChan (interface ^. #toGameChannel)
                        else return Nothing
    where
        chooser = opts ^. #owner

sendWinners :: (IOE :> es) => GameController l cn r ph pl i -> [Player] -> Eff es ()
sendWinners gc winners = liftIO $ traverse_ (sendWinners' winners) (gc ^. #playerInterfaces . to M.elems)
    where
        sendWinners' :: [Player] -> PlayerInterface l cn r ph pl i -> IO ()
        sendWinners' ps interface = writeChan (interface ^. #fromGameChannel) (SendWinners ps)


commonInterface :: [Player]
    -> Chan (GameToInterfacePayload l cn r ph pl i)
    -> Chan pl
    -> GameController l cn r ph pl i
commonInterface ps fromGameChan' toGameChan' =
    let theInterface = PlayerInterface fromGameChan' toGameChan'
    in GameController (M.fromList [(p, theInterface) | p <- ps])


agentToInterface :: Agent l cn r ph pl i IO -> PlayerInterface l cn r ph pl i
agentToInterface agent = PlayerInterface (agent ^. #fromGameChannel) (agent ^. #toGameChannel)

