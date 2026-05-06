{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Interface.Stdio
  ( InitMsg (..),
    StepMsg (..),
    InMsg (..),
    putJson,
    readAction,
    sendInit,
    encodeGameObjectsObs,
    runStdioAgent,
  )
where

import Control.Concurrent (Chan, MVar, readChan, withMVar, writeChan)
import Control.Lens ((^.))
import Control.Monad (forever)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, decodeStrict, withObject, (.:))
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import Data.Finitary (Finitary (..), inhabitants)
import qualified Data.Map as M
import Data.Proxy (Proxy (..))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Data.Generics.Labels ()
import FinitaryMap ((!!!))
import GHC.Generics (Generic)
import Game.Choose (GameToInterfacePayload (..))
import Game.GameState (GameState)
import Game.Location
  ( Counter (..),
    GymSpace (..),
    encodeCounterObs,
    encodeLocationObs,
    gameObjectsSpace,
  )
import Game.Options (Options (..), actionSpaceSize, decodeAction, legalActionIndices)
import Game.Player (Player (..))
import Game.View (GameObjectsView (..), GameStateView (..))

-- ---- Message types ----

data InitMsg = InitMsg
  { agents :: [Int],
    observationSpace :: GymSpace,
    actionSpace :: GymSpace
  }
  deriving (Generic, ToJSON)

data StepMsg = StepMsg
  { msgType :: Text,
    agent :: Int,
    observation :: Value,
    legalActions :: [Int],
    reward :: Float,
    terminated :: Bool,
    truncated :: Bool
  }
  deriving (Generic, ToJSON)

data InMsg
  = ActionMsg Int
  | ResetMsg
  deriving (Show)

instance FromJSON InMsg where
  parseJSON = withObject "InMsg" $ \o -> do
    t <- o .: "type" :: Parser Text
    case t of
      "action" -> ActionMsg <$> o .: "action"
      "reset"  -> pure ResetMsg
      _        -> fail $ "Unknown message type: " ++ T.unpack t

-- ---- I/O helpers ----

putJson :: (ToJSON a) => a -> IO ()
putJson = TIO.putStrLn . TL.toStrict . encodeToLazyText

readAction :: IO Int
readAction = do
  line <- BS.getLine
  case decodeStrict line of
    Just (ActionMsg i) -> return i
    _                  -> readAction

-- ---- Observation encoding ----

encodeGameObjectsObs
  :: forall l cn r. (Finitary l, Finitary cn, Finitary r, Ord r, Show l, Show cn)
  => GameObjectsView l cn r -> Value
encodeGameObjectsObs (GameObjectsView locsView cnsView) =
  toJSON $ M.fromList $
    [(T.pack (show l), encodeLocationObs (locsView !!! l)) | l <- inhabitants @l]
    ++ [(T.pack (show cn), encodeCounterObs (cnsView !!! cn)) | cn <- inhabitants @cn]

-- ---- Initialization ----

sendInit
  :: forall l cn r ph pl.
     (Finitary l, Finitary cn, Finitary r, Finitary pl, Show l, Show cn)
  => GameState l cn r ph pl -> IO ()
sendInit gs = putJson msg
  where
    toAgentNum (Player pnum) = fromEnum pnum
    agentNums = map toAgentNum (S.toList (gs ^. #players))
    obsSpace  = gameObjectsSpace (gs ^. #objects)
    actSpace  = GymDiscrete (actionSpaceSize (Proxy @pl))
    msg       = InitMsg agentNums obsSpace actSpace

-- ---- Stdio agent ----

-- One agent per player; agents share a stdout lock (MVar ()).
-- Ignores SendState and SendAnnouncement (not needed for training).
-- On SendOptions: emits a step message and reads one action from stdin.
-- On SendWinners: emits a terminal message with +1/-1 reward.
runStdioAgent
  :: forall l cn r ph pl.
     (Finitary l, Finitary cn, Finitary r, Finitary pl, Ord r, Show l, Show cn)
  => Player
  -> MVar ()
  -> [Player]
  -> Chan (GameToInterfacePayload l cn r ph pl)
  -> Chan pl
  -> IO ()
runStdioAgent thisPlayer lock _allPlayers fromChan toChan = forever $ do
  payload <- readChan fromChan
  case payload of
    SendState _          -> return ()
    SendAnnouncement _ _ -> return ()
    SendOptions gsv opts -> do
      let GameStateView _ objsView _ _ = gsv
      let Player agentPnum = opts ^. #owner
      let agentNum = fromEnum agentPnum
      let obs      = encodeGameObjectsObs objsView
      let legal    = legalActionIndices opts
      let msg      = StepMsg "step" agentNum obs legal 0.0 False False
      withMVar lock $ \_ -> putJson msg
      i <- readAction
      writeChan toChan (decodeAction i)
    SendWinners winners -> do
      let Player thisPnum = thisPlayer
      let agentNum = fromEnum thisPnum
      let reward   = if thisPlayer `elem` winners then 1.0 else -1.0
      let msg      = StepMsg "terminal" agentNum (toJSON (Nothing :: Maybe ())) [] reward True False
      withMVar lock $ \_ -> putJson msg
