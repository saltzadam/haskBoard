module Interface.Agent where

import Brick.BChan (BChan, readBChan, writeBChan)
import Control.Concurrent (Chan, readChan, writeChan)
import Control.Lens ((^.))
import Control.Monad.Random (forever, randomRIO)
import qualified Data.List.NonEmpty as NE
import Game.Agent
import Game.Choose
import Game.Options

-- Runs Agents, plus a few examples.

-- Start an agent
-- wait for a payload from fromChan
-- depending on the type, pull the appropriate handler
-- runAgentIO :: AgentM l cn r ph pl IO IO ()
runAgentIO :: Agent l cn r ph pl IO -> IO ()
runAgentIO agent = forever $ do
  let fromChan = agent ^. #fromGameChannel
  payload <- readChan fromChan
  -- let parsed = parsePayload payload
  case payload of
    SendState csv -> (agent ^. #stateHandler) csv
    SendWinners winners -> (agent ^. #winnersHandler) winners
    SendOptions gsv options -> do
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

--
-- chooses moves at random
randomAgent ::
  Chan (GameToInterfacePayload l cn r ph pl) ->
  Chan pl ->
  Agent l cn r ph pl IO
randomAgent fromGameChan toGameChan =
  Agent
    { playChooser = chooseRandom,
      stateHandler = \_ -> return (),
      winnersHandler = \_ -> return (),
      announceHandler = \_ _ -> return (),
      fromGameChannel = fromGameChan,
      toGameChannel = toGameChan
    }
  where
    chooseRandom :: p -> Options b -> IO b
    chooseRandom _ (Options legal _) =
      let numOptions = length legal
       in do
            choice <- randomRIO (1, numOptions)
            return (legal NE.!! (choice - 1))
