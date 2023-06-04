{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Game.Agent
    where
import Game.View (GameStateView)
import GHC.Generics
import Control.Lens (makeLenses, use, view)
import Control.Concurrent (Chan, readChan, writeChan)
import Game.Choose (GameToInterfacePayload (..))
import Control.Monad.State (StateT,  MonadState (..), MonadTrans (..),  evalStateT)
import Control.Monad (forever)
import Game.Options (Options)
import Game.Player (Player)


data Agent l cn r ph pl i m = Agent {
      playChooser :: GameStateView l cn r ph -> Options pl i -> m pl
    ,  stateHandler :: GameStateView l cn r ph -> m ()
                                    , winnersHandler :: [Player] -> m ()
    , fromGameChannel :: Chan (GameToInterfacePayload l cn r ph pl i)
    , toGameChannel :: Chan pl
                   } deriving (Generic)

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

