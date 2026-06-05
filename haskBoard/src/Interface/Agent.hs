module Interface.Agent where

import Brick.BChan (BChan, readBChan, writeBChan)
import Control.Concurrent (Chan, readChan, writeChan)
import Control.Lens ((^.))
import Control.Monad (forever)
import Control.Monad.Random (randomRIO)
import Data.Finitary (Finitary)
import Game.Agent
import Game.Choose
import Game.Options
import Game.View (GameStateView)
import qualified Data.Set.NonEmpty as NESet

-- | A hint suggests a play given the visible game state and legal options.
-- Return 'Just' to suggest a specific play, 'Nothing' to defer.
-- TODO: Surface hints in the TUI (e.g. highlight the suggested play).
type Hint l cn r ph pl = GameStateView l cn r ph -> Options pl -> Maybe pl

-- Runs Agents, plus a few examples.

-- Start an agent
-- wait for a payload from fromChan
-- depending on the type, pull the appropriate handler
-- runAgentIO :: AgentM l cn r ph pl IO IO ()
runAgentIO :: (Finitary l, Finitary cn, Show cn, Show l, Show r) => Agent l cn r ph pl IO -> IO ()
runAgentIO agent = forever $ do
  let fromChan = agent ^. #fromGameChannel
  payload <- readChan fromChan
  -- let parsed = parsePayload payload
  case payload of
    SendState gsv _scores -> (agent ^. #stateHandler) gsv
    SendWinners winners -> (agent ^. #winnersHandler) winners
    SendOptions gsv options ->
      do
        let chooser = agent ^. #playChooser
        let toChan = agent ^. #toGameChannel
        writeChan toChan =<< chooser gsv options
    SendAnnouncement speaker announcement -> (agent ^. #announceHandler) speaker announcement

termAgent ::
  (Show pl) =>
  Chan (GameToInterfacePayload l cn r ph pl) ->
  Chan pl ->
  Agent l cn r ph pl IO
termAgent fromGameChan toGameChan =
  Agent
    { playChooser = \_ options@(Options plays' _) -> do
        print options
        choice <- getLine
        return (foldr (:) [] plays' !! (read choice :: Int)),
      stateHandler = const (return ()),
      winnersHandler = const (return ()),
      announceHandler = \_ _ -> return (),
      fromGameChannel = fromGameChan,
      toGameChannel = toGameChan
    }

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
  [Hint l cn r ph pl] ->
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
    chooseWithHints gsv options@(Options legal _) =
      case applyHints hints gsv options of
        Just play -> return play
        Nothing ->
          let n = NESet.size legal
           in do
                i <- randomRIO (0, n - 1)
                return (foldr (:) [] legal !! i)
    applyHints [] _ _ = Nothing
    applyHints (h : hs) gsv opts =
      case h gsv opts of
        Just play -> Just play
        Nothing -> applyHints hs gsv opts
