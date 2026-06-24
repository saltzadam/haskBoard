{-# LANGUAGE OverloadedStrings #-}

module Run.Stdio
  ( sendInit,
    runStdioAgent,
  )
where

import Control.Concurrent (Chan, MVar, readChan, withMVar, writeChan)
import Control.Exception (IOException, catch)
import Control.Lens ((^.))
import Control.Monad (forever)
import Control.Monad.Random (randomRIO)
import Data.Aeson (ToJSON (..), decodeStrict)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Char8 as BS
import Data.Finitary (toFinite)
import Data.Generics.Labels ()
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Game.Choose (GameToInterfacePayload (..))
import Game.Constraints (GameCounter, GameLocation, GamePlay, GameResource)
import Game.GameState (GameRules, GameState)
import Game.Options (Options (..), decodeAction, legalActionIndices)
import Game.Player (Player (..))
import Game.View (GameObjectsView (..), GameStateView (..))
import Interface.Hint (HintM, applyHintsPure)
import Interface.Protocol (ActionSource (..), InMsg (..), InitMsg, StepMsg (..), addScoresToObs, buildInitMsg, encodeGameObjectsObs)
import System.Exit (exitSuccess)

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
  -- TODO: not much of a catch
  `catch` (\(_ :: IOException) -> exitSuccess)

-- ---- Initialization ----

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
