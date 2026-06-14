{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Interface.Stdio
  ( InitMsg (..),
    StepMsg (..),
    InMsg (..),
    ActionSource (..),
    putJson,
    readAction,
    buildInitMsg,
    sendInit,
    encodeGameObjectsObs,
    runStdioAgent,
  )
where

import Control.Concurrent (Chan, MVar, readChan, withMVar, writeChan)
import Control.Exception (IOException, catch)
import Control.Lens ((^.))
import Control.Monad.Random (randomRIO)
import System.Exit (exitSuccess)
import Control.Monad (forever)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), decodeStrict, withObject, (.:))
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as BS
import Data.Finitary (inhabitants, toFinite)
import Game.Constraints (GameCounter, GameLocation, GamePlay, GameResource)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map as M
import Data.Proxy (Proxy (..))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Data.Generics.Labels ()
import qualified Data.Set.NonEmpty as NESet
import FinitaryMap ((!!!))
import GHC.Generics (Generic)
import Game.Choose (GameToInterfacePayload (..))
import Interface.Hint (HintM, applyHintsPure)
import Game.GameState (GameRules, GameState)
import Data.Map (Map)
import Game.Location
  (
    GymSpace (..),
    NormHint (..),
    encodeCounterObs,
    encodeLocationObs,
    inventoryTotals,
  )
import Game.Options (Options (..), actionSpaceSize, decodeAction, legalActionIndices)
import Game.Player (Player (..))
import Game.View (GameObjectsView (..), GameStateView (..), gameObjectsViewSpace, viewGameStateAs')
import Interface.Hint (applyHints, HintM)
import Effectful (runEff)

-- ---- Message types ----

data ActionSource = Hint | Random | Agent | Human
  deriving (Show, Generic, ToJSON)

data InitMsg = InitMsg
  { agents :: [Int],
    observationSpaces :: M.Map Int GymSpace,
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
    truncated :: Bool,
    hintAction :: Maybe Int,
    actionSource :: ActionSource
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
readAction =
  (do
    line <- BS.getLine
    case decodeStrict line of
      Just (ActionMsg i) -> return i
      _                  -> readAction)
  `catch` (\(_ :: IOException) -> exitSuccess)

-- ---- Observation encoding ----

encodeGameObjectsObs
  :: forall l cn r. (GameLocation l, GameCounter cn, GameResource r)
  => Map r Int -> GameObjectsView l cn r -> Value
encodeGameObjectsObs totals (GameObjectsView locsView cnsView) =
  toJSON $ M.fromList $
    [(T.pack (show l), encodeLocationObs totals (Just loc)) | l <- inhabitants @l, Just loc <- [locsView !!! l]]
    ++ [(T.pack (show cn), encodeCounterObs (Just c)) | cn <- inhabitants @cn, Just c <- [cnsView !!! cn]]

-- | Merge a "scores" array into an existing observation Value (a JSON object).
-- When isPublic=True, the array contains all players' scores (ascending player order).
-- When isPublic=False, it contains only thisPlayer's score (length 1).
addScoresToObs :: M.Map Player Int -> Bool -> Player -> Value -> Value
addScoresToObs allScores isPublic thisPlayer (Object km) =
  let scoreList = if isPublic
                  then map snd (M.toAscList allScores)
                  else case M.lookup thisPlayer allScores of
                         Nothing -> []
                         Just s  -> [s]
  in Object (KM.insert (fromText "scores") (toJSON scoreList) km)
addScoresToObs _ _ _ v = v

-- ---- Initialization ----

buildInitMsg
  :: forall l cn r ph pl.
     (GameLocation l, GameCounter cn, GameResource r, GamePlay pl)
  => GameState l cn r ph pl -> GameRules l cn r ph pl -> InitMsg
buildInitMsg gs gr = InitMsg agentNums obsSpaces actSpace
  where
    toAgentNum (Player pnum) = fromEnum pnum
    players    = S.toList (gs ^. #players)
    agentNums  = map toAgentNum players
    totals     = inventoryTotals (gs ^. #objects ^. #locations)
    (lo, hi)   = gr ^. #scoreBounds
    isPublic   = gr ^. #scorePublic
    numPlayers = length players
    scoreCount p = if isPublic then numPlayers else const 1 p
    scoreSpace p  = GymBox (fromIntegral lo) (fromIntegral hi) [scoreCount p] MinMax
    addScores p (GymDict pairs) = GymDict (pairs ++ [("scores", scoreSpace p)])
    addScores _ other = other
    obsSpaces = M.fromList
      [ (toAgentNum p, addScores p (gameObjectsViewSpace totals (viewGameStateAs' gs p ^. #objectsView)))
      | p <- players ]
    actSpace  = GymDiscrete (actionSpaceSize (Proxy @pl)) NoNorm

sendInit
  :: forall l cn r ph pl.
     (GameLocation l, GameCounter cn, GameResource r, GamePlay pl)
  => GameState l cn r ph pl -> GameRules l cn r ph pl -> IO ()
sendInit gs gr = putJson (buildInitMsg gs gr)

-- ---- Stdio agent ----

-- One agent per player; agents share a stdout lock (MVar ()).
-- Caches scores from SendState; merges them into obs on SendOptions.
-- On SendWinners: emits a terminal message with zero-sum reward.
runStdioAgent
  :: forall l cn r ph pl.
     (GameLocation l, GameCounter cn, GameResource r, GamePlay pl)
  => Map r Int
  -> [HintM l cn r ph pl]
  -> Bool
  -> Player
  -> MVar ()
  -> [Player]
  -> GameRules l cn r ph pl
  -> Chan (GameToInterfacePayload l cn r ph pl)
  -> Chan pl
  -> IO ()
runStdioAgent totals hints selfPlay thisPlayer lock allPlayers gr fromChan toChan = do
  scoreRef <- newIORef (M.empty :: M.Map Player Int)
  forever $ do
    payload <- readChan fromChan
    case payload of
      SendState _ scores   -> writeIORef scoreRef scores
      SendAnnouncement _ _ -> return ()
      SendOptions gsv opts -> do
        let GameStateView _ objsView _ _ _ = gsv
        let Player agentPnum = opts ^. #owner
        let agentNum = fromEnum agentPnum
        scores <- readIORef scoreRef
        let obs      = addScoresToObs scores (gr ^. #scorePublic) thisPlayer
                         (encodeGameObjectsObs totals objsView)
        let legal    = legalActionIndices opts
        if selfPlay
          then do
            let hintResult = applyHintsPure gsv hints opts
            chosenPlay <- case hintResult of
              Just play -> return play
              Nothing -> do
                let n = NESet.size (opts ^. #legal)
                i <- randomRIO (0, n - 1)
                return (foldr (:) [] (opts ^. #legal) !! i)
            let chosenIdx = fromIntegral (toFinite chosenPlay)
            let source = maybe Random (const Hint) hintResult
            let msg = StepMsg "step" agentNum obs legal 0.0 False False (Just chosenIdx) source
            withMVar lock $ \_ -> putJson msg
            writeChan toChan chosenPlay
          else do
            let msg = StepMsg "step" agentNum obs legal 0.0 False False Nothing Agent
            withMVar lock $ \_ -> putJson msg
            i <- readAction
            writeChan toChan (decodeAction i)
      SendWinners winners -> do
        let Player thisPnum = thisPlayer
        let agentNum = fromEnum thisPnum
        let n        = length allPlayers
        let nWinners = length (filter (`elem` winners) allPlayers)
        let nLosers  = n - nWinners
        let reward   = if thisPlayer `elem` winners
                        then fromIntegral nLosers / fromIntegral n
                        else -(fromIntegral nWinners / fromIntegral n) :: Float
        let msg      = StepMsg "terminal" agentNum (toJSON (Nothing :: Maybe ())) [] reward True False Nothing Agent
        withMVar lock $ \_ -> putJson msg
