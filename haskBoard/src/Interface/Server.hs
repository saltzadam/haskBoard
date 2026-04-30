{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Interface.Server where

import Control.Concurrent (MVar, forkIO, modifyMVar, modifyMVar_, newMVar, putMVar, readChan, readMVar, threadDelay, writeChan)
import Control.Concurrent.Async (forConcurrently_, withAsync)
import Control.Exception (finally)
import Control.Lens (makeFields, over, (^.))
import Control.Monad (forM_, forever, void)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON (..), ToJSON (..), ToJSONKey, decode)
import Data.Aeson.Text (encodeToLazyText)
import Data.Finitary (Finitary)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Debug.Trace as Debug
import GHC.Generics (Generic)
import Game.Choose (GameToInterfacePayload (..))
import Game.Player (Player (..), PlayerNum, mkPlayers)
import Interface.Controller (GameController, PlayerInterface (..))
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

newServerState :: ServerState
newServerState = ServerState M.empty NotEnoughClients 3

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
  (Finitary l, Finitary cn, Finitary r, Ord l, Ord cn, ToJSONKey r, ToJSONKey l, ToJSONKey cn, ToJSON ph, ToJSON l, ToJSON r, ToJSON cn, ToJSON pl, FromJSON pl) =>
  GameController l cn r ph pl ->
  IO ()
server controller = do
  print "init server"
  state <- newMVar newServerState
  print "got state"
  WS.runServer "127.0.0.1" 9159 $ (application controller) state

application ::
  (Finitary l, Finitary cn, Finitary r, Ord l, Ord cn, ToJSONKey r, ToJSONKey l, ToJSONKey cn, ToJSON ph, ToJSON l, ToJSON r, ToJSON cn, ToJSON pl, FromJSON pl) =>
  GameController l cn r ph pl ->
  MVar ServerState ->
  PendingConnection ->
  IO ()
application controller state pending = do
  print "init application"
  Debug.traceM "init application"
  conn <- WS.acceptRequest pending
  print "got a connection"
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
    print msg

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
              print player'
              WS.sendTextData conn $
                "Welcome! Players: "
                  <> T.intercalate ", " (T.pack . show <$> M.keys (s' ^. #clients))
              print (M.keys (s' ^. #clients))
              print (T.pack (show player') <> " joined")
              broadcast (T.pack (show (fst client)) <> " joined") s'
              return s'

            forever $
              ifM
                ((\ss -> ss ^. #serverStatus == Active) <$> readMVar state)
                ( print "main loop true"
                    >> playerWorker controller player' state
                )
                (WS.sendTextData conn ("waiting for more players" :: Text) >> threadDelay 5000000)
  where
    disconnect pNum = do
      -- Remove client and return new state
      let player' = Player pNum
      print ("disconnecting" :: Text)
      s <- modifyMVar state $ \s ->
        let s' = removeClient player' s in return (s', s')
      broadcast (T.pack (show player') <> " disconnected") s

updateStatus :: MVar ServerState -> ServerStatus -> IO ()
updateStatus state' status =
  modifyMVar_ state' (\s -> return s {serverStatus = status})

withWorker :: IO a -> IO a -> IO a
withWorker outer inner = withAsync outer $ const inner

runGame ::
  (Finitary l, Finitary cn, Finitary r, Ord l, Ord cn, ToJSONKey r, ToJSONKey l, ToJSONKey cn, ToJSON ph, ToJSON l, ToJSON r, ToJSON cn, ToJSON pl, FromJSON pl) =>
  GameController l cn r ph pl ->
  MVar ServerState ->
  IO ()
runGame controller ss = do
  print ("init rungame")
  ss' <- readMVar ss
  let players = mkPlayers (ss' ^. #expectedPlayers)
  foldr withWorker (return ()) ((\p -> playerWorker controller p ss) <$> players)

exit :: e -> ExceptT e IO a
exit = ExceptT . return . Left

playerWorker ::
  (Finitary l, Finitary cn, Finitary r, Ord l, Ord cn, ToJSONKey r, ToJSONKey l, ToJSONKey cn, ToJSON ph, ToJSON l, ToJSON r, ToJSON cn, ToJSON pl, FromJSON pl) =>
  GameController l cn r ph pl ->
  Player ->
  MVar ServerState ->
  IO ()
playerWorker controller p ss = forever $ do
  print ("init playerWorker")
  ss' <- readMVar ss
  let conn = (ss' ^. #clients) M.! p
  let PlayerInterface fromGameChan toGameChan = (controller ^. #playerInterfaces) M.! p
  fromGameMsg <- readChan fromGameChan
  -- WS.sendTextData conn (encodeToLazyText fromGameMsg)
  case fromGameMsg of
    SendState gsv -> do
      let gsvJSON = toJSON gsv
      WS.sendTextData conn (encodeToLazyText gsvJSON)
    SendOptions _ _ -> do
      void $ runExceptT $ forever $ do
        -- loop with break
        response <- lift (WS.receiveData conn)
        case decode response of
          Just (responseEncoded :: pl) -> lift (writeChan toGameChan responseEncoded) >> exit ()
          Nothing -> lift (WS.sendTextData conn ("invalid play" :: Text))
    _ -> return ()
