module Interface.Agent (runAgentIO, brickAgent, randomAgent) where

import Brick.BChan (BChan, readBChan, writeBChan)
import Control.Concurrent (Chan, readChan, writeChan)
import Control.Lens ((^.))
import Control.Monad (forever)
import Control.Monad.Random (randomRIO)
import Data.Finitary (Finitary)
import Game.Agent
import Game.Choose
import Game.Options
import Game.View (GameStateView, inject)
import qualified Data.Set.NonEmpty as NESet
import Interface.Hint (applyHints, HintM)
import Effectful.State.Static.Shared (evalState)
import Effectful (runEff)

runAgentIO :: (Finitary l, Finitary cn, Show cn, Show l, Show r) => Agent l cn r ph pl IO -> IO ()
runAgentIO agent = forever $ do
  let fromChan = agent ^. #fromGameChannel
  payload <- readChan fromChan
  case payload of
    SendState gsv _scores -> (agent ^. #stateHandler) gsv
    SendWinners winners -> (agent ^. #winnersHandler) winners
    SendOptions gsv options ->
      do
        let chooser = agent ^. #playChooser
        let toChan = agent ^. #toGameChannel
        writeChan toChan =<< chooser gsv options
    SendAnnouncement speaker announcement -> (agent ^. #announceHandler) speaker announcement

brickAgent ::
  Chan (GameToInterfacePayload l cn r ph pl) ->
  BChan (BEvent l cn r ph pl) ->
  Chan pl ->
  BChan pl ->
  Agent l cn r ph pl IO
brickAgent fromGameChan toBrickBChan toGameChan fromBrickBChan =
  Agent
    { playChooser = \_ options -> do
        writeBChan toBrickBChan (Request options)
        readBChan fromBrickBChan,
      stateHandler = writeBChan toBrickBChan . Receive,
      winnersHandler = writeBChan toBrickBChan . AnnounceWinner,
      announceHandler = \speaker announcement -> writeBChan toBrickBChan (AnnounceEvent speaker announcement),
      fromGameChannel = fromGameChan,
      toGameChannel = toGameChan
    }

-- | Agent that applies hints in order, falling back to random choice.
-- Pass @[]@ for a purely random agent.
randomAgent ::
  (Eq l, Eq cn) => [HintM l cn r ph pl] ->
  Chan (GameToInterfacePayload l cn r ph pl) ->
  Chan pl ->
  Agent l cn r ph pl IO
randomAgent hints fromGameChan toGameChan =
  Agent
    { playChooser = chooseWithHints,
      stateHandler = \_ -> return (),
      winnersHandler = \_ -> return (),
      announceHandler = \_ _ -> return (),
      fromGameChannel = fromGameChan,
      toGameChannel = toGameChan
    }
  where
    chooseWithHints gsv options@(Options legal _) = do
      hintedChoice <- runEff . evalState (inject gsv) $ applyHints hints options
      case hintedChoice of
        Just y -> return y
        Nothing -> do
          let n = NESet.size legal
          i <- randomRIO (0,n-1)
          return (foldr (:) [] legal !! i)
