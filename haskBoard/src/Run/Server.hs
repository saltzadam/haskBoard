{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Run.Server
  ( server,
    spawnRLLibAgent,
  )
where

import Control.Concurrent (Chan, MVar, modifyMVar, modifyMVar_, newChan, newMVar, putMVar, readChan, readMVar, threadDelay, writeChan)
import System.Directory (makeAbsolute)
import System.FilePath (takeDirectory)
import System.Process (ProcessHandle, createProcess, proc, std_out, std_err, StdStream(..))
import System.IO (openFile, IOMode(..))
import Control.Concurrent.Async (withAsync)
import Control.Exception (finally)
import Control.Lens (makeFields, over, (^.))
import Control.Monad (forM_, forever, void)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans (lift)
import Data.Aeson (ToJSON (..), Value (..), decode, toJSON)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import Game.Constraints (GameCounter, GameLocation, GamePhase, GamePlay, GameResource)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Game.Choose (GameToInterfacePayload (..))
import Game.GameState (GameRules, GameState)
import Game.Options (decodeAction, legalActionIndices)
import Game.Player (Player (..), PlayerNum)
import Game.View (GameStateView (..))
import Interface.Controller (GameController, PlayerInterface (..))
import Interface.Protocol (ActionSource (..), InMsg (..), StepMsg (..), buildInitMsg, encodeGameObjectsObs)
import Network.WebSockets as WS
import Text.Read (readMaybe)
import Util (ifM, forkIO_)

data ServerStatus
  = NotEnoughClients
  | Active
  deriving (Eq, Ord, Show)

data ServerState = ServerState
  { clients :: Map Player Connection,
    serverStatus :: ServerStatus,
    expectedPlayers :: Int,
    readyPlayers :: Int
  }
  deriving (Generic)

makeFields ''ServerState

newServerState :: Int -> ServerState
newServerState n = ServerState M.empty NotEnoughClients n 0


clientExists :: Player -> ServerState -> Bool
clientExists client server_ = client `elem` M.keys (server_ ^. #clients)

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
  Map r Int ->
  Int ->
  GameState l cn r ph pl ->
  GameRules l cn r ph pl ->
  GameController l cn r ph pl ->
  Maybe (MVar ()) ->
  IO ()
server totals numPlayers gs gr controller mReady = do
  let waitReady = case mReady of Just _ -> True; Nothing -> False
  state <- newMVar (newServerState numPlayers)
  forM_ mReady $ \readyVar ->
    forkIO_ $ waitForActive state readyVar
  WS.runServer "127.0.0.1" 9159 $ application waitReady totals gs gr controller state

waitForActive :: MVar ServerState -> MVar () -> IO ()
waitForActive state readyVar = do
  ss <- readMVar state
  if ss ^. #serverStatus == Active
    then putMVar readyVar ()
    else threadDelay 50000 >> waitForActive state readyVar

application ::
  forall l cn r ph pl.
  (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl) =>
  Bool ->
  Map r Int ->
  GameState l cn r ph pl ->
  GameRules l cn r ph pl ->
  GameController l cn r ph pl ->
  MVar ServerState ->
  PendingConnection ->
  IO ()
application waitReady totals gs gr controller state pending = do
  conn <- WS.acceptRequest pending
  -- Monitor readyPlayers count; set Active when all expected players are ready
  forkIO_ $
      void $
        runExceptT $
          forever $
            ifM
              (lift ((\ss -> (ss ^. #expectedPlayers) == (ss ^. #readyPlayers)) <$> readMVar state))
              (lift (modifyMVar_ state (\ss -> return (ss {serverStatus = Active}))) >> exit ())
              (return ())

  WS.withPingThread conn 30 (return ()) $ do
    msg <- WS.receiveData conn
    case toEnum <$> (readMaybe (T.unpack msg) :: Maybe Int) of
      Nothing -> WS.sendTextData conn ("Not a player number" :: Text)
      Just playerNum -> flip finally (disconnect playerNum) $ do
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
            let initMsg = buildInitMsg gs gr
            WS.sendTextData conn (encodeToLazyText (toJSON initMsg))

            if waitReady
              then do
                -- Wait for {"type":"ready"} from client before marking ready
                awaitReadyMsg conn
                modifyMVar_ state $ \ss ->
                  return ss { readyPlayers = (ss ^. #readyPlayers) + 1 }
                playerWorker totals controller player' state
              -- TODO: why doesn't this wait for more players?
              else do
                -- Mark ready immediately (connection = ready)
                modifyMVar_ state $ \ss ->
                  return ss { readyPlayers = (ss ^. #readyPlayers) + 1 }
                forever $
                  ifM
                    ((\ss -> ss ^. #serverStatus == Active) <$> readMVar state)
                    (playerWorker totals controller player' state)
                    (WS.sendTextData conn ("waiting for more players" :: Text) >> threadDelay 100000)
  where
    disconnect pNum = do
      let player' = Player pNum
      s <- modifyMVar state $ \s ->
        let s' = removeClient player' s in return (s', s')
      broadcast (T.pack (show player') <> " disconnected") s

-- | Block until the WS client sends {"type":"ready"}.
-- Non-ready messages (broadcasts, state updates) are silently discarded.
awaitReadyMsg :: Connection -> IO ()
awaitReadyMsg conn = do
  raw <- WS.receiveData conn :: IO LBS.ByteString
  case decode raw :: Maybe (Map Text Value) of
    Just m | M.lookup "type" m == Just (String "ready") -> return ()
    _ -> awaitReadyMsg conn

exit :: e -> ExceptT e IO a
exit = ExceptT . return . Left

playerWorker ::
  (GameLocation l, GameCounter cn, GameResource r, GamePhase ph, GamePlay pl) =>
  Map r Int ->
  GameController l cn r ph pl ->
  Player ->
  MVar ServerState ->
  IO ()
playerWorker totals controller p ss = do
  ss' <- readMVar ss
  let conn = (ss' ^. #clients) M.! p
  let PlayerInterface fromGameChan toGameChan = (controller ^. #playerInterfaces) M.! p
  -- A dedicated reader thread keeps the WebSocket alive by continuously
  -- calling receiveData, which processes ping/pong control frames even
  -- when the main loop is blocked waiting for game-channel messages.
  -- Data frames (action responses) are forwarded through actionChan.
  actionChan <- newChan
  withAsync (wsReader conn actionChan) $ \_ ->
    forever $ do
      fromGameMsg <- readChan fromGameChan
      case fromGameMsg of
        SendState gsv _scores -> do
          let gsvJSON = toJSON gsv
          WS.sendTextData conn (encodeToLazyText gsvJSON)
        SendOptions gsv opts -> do
          let GameStateView _ objsView _ _ _ = gsv
              Player agentPnum = opts ^. #owner
              agentNum = fromEnum agentPnum
              obs   = encodeGameObjectsObs totals objsView
              legal = legalActionIndices opts
              msg   = StepMsg "step" agentNum obs legal 0.0 False False Nothing Agent
          WS.sendTextData conn (encodeToLazyText (toJSON msg))
          waitForAction actionChan toGameChan
        SendWinners winners -> do
          let Player thisPnum = p
              agentNum = fromEnum thisPnum
              allPlayers = M.keys (controller ^. #playerInterfaces)
              n        = length allPlayers
              nWinners = length (filter (`elem` winners) allPlayers)
              nLosers  = n - nWinners
              reward   = if p `elem` winners
                          then fromIntegral nLosers / fromIntegral n
                          else -(fromIntegral nWinners / fromIntegral n) :: Float
              msg      = StepMsg "terminal" agentNum (toJSON (Nothing :: Maybe ())) [] reward True False Nothing Agent
          WS.sendTextData conn (encodeToLazyText (toJSON msg))
        _ -> return ()

-- | Background thread that continuously reads from the WebSocket.
-- Ensures ping frames are ponged even when playerWorker is idle.
wsReader :: Connection -> Chan InMsg -> IO ()
wsReader conn actionChan = forever $ do
  raw <- WS.receiveData conn :: IO LBS.ByteString
  case decode raw of
    Just msg -> writeChan actionChan msg
    Nothing  -> return ()

-- | Read from the action channel until we get an ActionMsg.
waitForAction ::
  (GamePlay pl) =>
  Chan InMsg -> Chan pl -> IO ()
waitForAction actionChan toGameChan = do
  msg <- readChan actionChan
  case msg of
    ActionMsg i -> writeChan toGameChan (decodeAction i)
    _           -> waitForAction actionChan toGameChan

-- | Spawn a Python WebSocket agent that connects to the server and plays
-- using a trained RLlib checkpoint (no Ray cluster needed).
-- Stdout/stderr are redirected to errors.log to avoid corrupting the Brick TUI.
spawnRLLibAgent
  :: FilePath     -- ^ Path to ws_agent_rllib.py
  -> FilePath     -- ^ Path to RLlib checkpoint directory
  -> PlayerNum    -- ^ Player slot to fill
  -> IO ProcessHandle
spawnRLLibAgent scriptPath checkpointPath playerNum = do
  absScript <- makeAbsolute scriptPath
  absCheckpoint <- makeAbsolute checkpointPath
  let projectDir = takeDirectory absScript
  logH <- openFile "errors.log" AppendMode
  let cp = (proc "uv"
              [ "run", "--project", projectDir, "python", absScript
              , "--checkpoint", absCheckpoint
              , "--player", show (fromEnum playerNum)
              ]) { std_out = UseHandle logH, std_err = UseHandle logH }
  (_, _, _, ph) <- createProcess cp
  return ph
