{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
module Game.Agent where

import Brick.BChan (BChan, readBChan, writeBChan)
import Control.Concurrent (Chan, readChan, writeChan)
import Control.Lens (makeLenses, use, view)
import Control.Monad (forever)
import Control.Monad.Random (randomRIO)
import Control.Monad.State (MonadState (..), MonadTrans (..), StateT, evalStateT)
import qualified Data.List.NonEmpty as NE
import GHC.Generics
import Game.Options (Options (..))
import Game.Player (Player)
import Game.View (GameStateView)
import Interface.Choose (GameToInterfacePayload (..))

data BEvent l cn r ph pl i
  = Receive (GameStateView l cn r ph)
  | Request (Options pl i)
  | Answer pl
  | AnnounceWinner [Player]
  deriving (Generic)

extractReceive :: BEvent l cn r ph pl i -> Maybe (GameStateView l cn r ph)
extractReceive (Receive gsv) = Just gsv
extractReceive _ = Nothing

instance (Show pl, Show i) => Show (BEvent l cn r ph pl i) where
  show (Receive g) = "Receive"
  show (Request opts) = "Request (" ++ show opts ++ ")"
  show (Answer play) = "Answer (" ++ show play ++ ")"
  show (AnnounceWinner winners) = show (head winners) ++ " is the winner!"

-- TODO: just make agents handle events

data Agent l cn r ph pl i m = Agent
  { playChooser :: GameStateView l cn r ph -> Options pl i -> m pl,
    stateHandler :: GameStateView l cn r ph -> m (),
    winnersHandler :: [Player] -> m (),
    fromGameChannel :: Chan (GameToInterfacePayload l cn r ph pl i),
    toGameChannel :: Chan pl
  }
  deriving (Generic)

newtype AgentM l cn r ph pl i m n a = AgentM {runAgentM :: StateT (Agent l cn r ph pl i m) n a}
  deriving (Generic, Functor, Applicative, Monad, MonadState (Agent l cn r ph pl i m), MonadTrans)

makeLenses ''Agent
makeLenses ''AgentM

runFromAgentIO :: Agent l cn r ph pl i IO -> IO ()
runFromAgentIO = evalStateT (view #runAgentM runAgentIO)

runAgentIO :: AgentM l cn r ph pl i IO IO ()
runAgentIO = forever $ do
  fromChan <- use #fromGameChannel
  payload <- lift $ readChan fromChan
  -- let parsed = parsePayload payload
  case payload of
    SendState csv -> do
      handler <- use #stateHandler
      lift $ handler csv
    SendWinners winners -> do
      handler <- use #winnersHandler
      lift $ handler winners
    SendOptions gsv options -> do
      chooser <- use #playChooser
      toChan <- use #toGameChannel
      lift $ writeChan toChan =<< chooser gsv options

brickAgent ::
  Chan
    ( GameToInterfacePayload
        l
        cn
        r
        ph
        pl
        i
    ) ->
  BChan (BEvent l cn r ph pl i) ->
  Chan pl ->
  BChan pl ->
  Agent
    l
    cn
    r
    ph
    pl
    i
    IO
brickAgent fromGameChan toBrickBChan toGameChan fromBrickBChan =
  Agent
    { playChooser = \_ options -> do
        writeBChan toBrickBChan (Request options)
        readBChan fromBrickBChan,
      stateHandler = \gsv -> writeBChan toBrickBChan (Receive gsv),
      winnersHandler = \winners -> writeBChan toBrickBChan (AnnounceWinner winners),
      fromGameChannel = fromGameChan,
      toGameChannel = toGameChan
    }

--
-- chooses moves at random
randomAgent ::
  Chan
    ( GameToInterfacePayload
        l
        cn
        r
        ph
        pl
        i
    ) ->
  Chan pl ->
  Agent
    l
    cn
    r
    ph
    pl
    i
    IO
randomAgent fromGameChan toGameChan =
  Agent
    { playChooser = chooseRandom,
      stateHandler = \_ -> return (),
      winnersHandler = \_ -> return (),
      fromGameChannel = fromGameChan,
      toGameChannel = toGameChan
    }

chooseRandom :: p -> Options b i -> IO b
chooseRandom _ (Options legal _ _) =
  let numOptions = length legal
   in do
        choice <- randomRIO (1, numOptions)
        return (legal NE.!! (choice - 1))
