{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Interface.Server where

import Control.Concurrent (MVar, forkIO, modifyMVar, modifyMVar_, newMVar, readChan, readMVar, threadDelay, writeChan)
import Control.Concurrent.Async (withAsync)
import Control.Exception (finally)
import Control.Lens (makeFields, over, (^.))
import Control.Monad (forM_, forever, void)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON (..), ToJSON (..), ToJSONKey, decode, toJSON)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as LBS
import Data.Finitary (Finitary)
import Data.Map (Map)
import Game.Constraints (GameCounter, GameLocation, GamePhase, GamePlay, GameResource)
import qualified Data.Map as M
import Data.Proxy (Proxy (..))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Game.Choose (GameToInterfacePayload (..))
import Game.GameState (GameState)
import Game.Location (GymSpace (..))
import Game.Options (actionSpaceSize, decodeAction, legalActionIndices)
import Game.Player (Player (..), PlayerNum, mkPlayers)
import Game.View (GameStateView (..), gameObjectsViewSpace, viewGameStateAs')
import Interface.Controller (GameController, PlayerInterface (..))
import Interface.Stdio (InitMsg (..), InMsg (..), StepMsg (..), encodeGameObjectsObs)
import Network.WebSockets as WS
import Text.Read (readMaybe)
import Util (ifM)

data ServerStatus
  = NotEnoughClients
  | Active
  deriving (Eq, Ord, Show)

data ServerState = ServerState
  { clients :: Map Player Connection,
    serverStatus :: ServerStatus,
    expectedPlayers :: Int
  }
  deriving (Generic)

makeFields ''ServerState

newServerState :: Int -> ServerState
newServerState n = ServerState M.empty NotEnoughClients n

serverNumPlayers :: ServerState -> Int
serverNumPlayers server_ = length (server_ ^. #clients)

clientExists :: Player -> ServerState -> Bool
clientExists client server_ = any (== client) (M.keys $ server_ ^. #clients)

addClient' :: Player -> Connection -> ServerState -> ServerState
addClient' p conn = over #clients (M.insert p conn)

addClient :: Player -> Connection -> ServerState -> ServerState
addClient p conn server_ = if clientExists p server_ then server_ else addClient' p conn server_

removeClient :: Player -> ServerState -> ServerState
removeClient p = over #clients (M.delete p)

broadcast :: Text -> ServerState -> IO ()
broadcast message server_ = do
  forM_ (M.toList $ server_ ^. #clients) $ \(_, conn) -> WS.sendTextData conn message

server ::
  (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl) =>
  Int ->
  GameState l cn r ph pl ->
  GameController l cn r ph pl ->
  IO ()
server numPlayers gs controller = do
  state <- newMVar (newServerState numPlayers)
  WS.runServer "127.0.0.1" 9159 $ application gs controller state

application ::
  forall l cn r ph pl.
  (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl) =>
  GameState l cn r ph pl ->
  GameController l cn r ph pl ->
  MVar ServerState ->
  PendingConnection ->
  IO ()
application gs controller state pending = do
  conn <- WS.acceptRequest pending
  void $
    forkIO $
      void $
        runExceptT $
          forever $
            ifM
              (lift ((\ss -> (ss ^. #expectedPlayers) == (serverNumPlayers ss)) <$> readMVar state))
              (lift ((modifyMVar_ state (\ss -> return (ss {serverStatus = Active})))) >> exit ())
              (return ())

  WS.withPingThread conn 30 (return ()) $ do
    -- When a client is succesfully connected, we read the first message. This should
    -- be in the format of "Hi! I am Jasper", where Jasper is the requested username.
    msg <- WS.receiveData conn
    case readMaybe (T.unpack msg) :: Maybe PlayerNum of
      -- Check that the first message has the right format:
      Nothing -> WS.sendTextData conn ("Not a player number" :: Text)
      Just playerNum
        -- Check that the player is not already taken:
        -- All is right! We're going to allow the client, but for safety reasons we *first*
        -- setup a `disconnect` function that will be run when the connection is closed.
        | otherwise -> flip finally (disconnect playerNum) $ do
            -- We send a "Welcome!", according to our own little protocol. We add the client to
            -- the list and broadcast the fact that he has joined. Then, we give control to the
            -- 'talk' function.
            let player' = Player playerNum
            let client = (player', conn)
            modifyMVar_ state $ \s -> do
              let s' = addClient player' (snd client) s
              WS.sendTextData conn $
                "Welcome! Players: "
                  <> T.intercalate ", " (T.pack . show <$> M.keys (s' ^. #clients))
              broadcast (T.pack (show (fst client)) <> " joined") s'
              return s'

            -- Send InitMsg so clients can build obs/act spaces
            let allNums   = map (\(Player pn) -> fromEnum pn) (S.toList (gs ^. #players))
                objsView  = (viewGameStateAs' gs player') ^. #objectsView
                obsSpace  = gameObjectsViewSpace objsView
                actSpace  = GymDiscrete (actionSpaceSize (Proxy @pl))
                initMsg   = InitMsg allNums obsSpace actSpace
            WS.sendTextData conn (encodeToLazyText (toJSON initMsg))

            forever $
              ifM
                ((\ss -> ss ^. #serverStatus == Active) <$> readMVar state)
                (playerWorker controller player' state)
                (WS.sendTextData conn ("waiting for more players" :: Text) >> threadDelay 5000000)
  where
    disconnect pNum = do
      -- Remove client and return new state
      let player' = Player pNum
      s <- modifyMVar state $ \s ->
        let s' = removeClient player' s in return (s', s')
      broadcast (T.pack (show player') <> " disconnected") s

updateStatus :: MVar ServerState -> ServerStatus -> IO ()
updateStatus state' status =
  modifyMVar_ state' (\s -> return s {serverStatus = status})

withWorker :: IO a -> IO a -> IO a
withWorker outer inner = withAsync outer $ const inner

runGame ::
  (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl) =>
  GameController l cn r ph pl ->
  MVar ServerState ->
  IO ()
runGame controller ss = do
  ss' <- readMVar ss
  let players = mkPlayers (ss' ^. #expectedPlayers)
  foldr withWorker (return ()) ((\p -> playerWorker controller p ss) <$> players)

exit :: e -> ExceptT e IO a
exit = ExceptT . return . Left

playerWorker ::
  (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl) =>
  GameController l cn r ph pl ->
  Player ->
  MVar ServerState ->
  IO ()
playerWorker controller p ss = forever $ do
  ss' <- readMVar ss
  let conn = (ss' ^. #clients) M.! p
  let PlayerInterface fromGameChan toGameChan = (controller ^. #playerInterfaces) M.! p
  fromGameMsg <- readChan fromGameChan
  case fromGameMsg of
    SendState gsv -> do
      let gsvJSON = toJSON gsv
      WS.sendTextData conn (encodeToLazyText gsvJSON)
    SendOptions gsv opts -> do
      let GameStateView _ objsView _ _ = gsv
          Player agentPnum = opts ^. #owner
          agentNum = fromEnum agentPnum
          obs   = encodeGameObjectsObs objsView
          legal = legalActionIndices opts
          msg   = StepMsg "step" agentNum obs legal 0.0 False False
      WS.sendTextData conn (encodeToLazyText (toJSON msg))
      void $ runExceptT $ forever $ do
        response <- lift (WS.receiveData conn :: IO LBS.ByteString)
        case decode response of
          Just (ActionMsg i) -> lift (writeChan toGameChan (decodeAction i)) >> exit ()
          _                  -> lift $ WS.sendTextData conn ("invalid play" :: Text)
    SendWinners winners -> do
      let Player thisPnum = p
          agentNum = fromEnum thisPnum
          reward   = if p `elem` winners then 1.0 else -1.0 :: Float
          msg      = StepMsg "terminal" agentNum (toJSON (Nothing :: Maybe ())) [] reward True False
      WS.sendTextData conn (encodeToLazyText (toJSON msg))
    _ -> return ()
