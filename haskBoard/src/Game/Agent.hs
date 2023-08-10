{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
module Game.Agent where

import Control.Concurrent (Chan)
import Control.Lens (makeLenses)
import Control.Monad.State (MonadState (..), MonadTrans (..), StateT, evalStateT)
import GHC.Generics
import Game.Choose (GameToInterfacePayload)
import Game.Options (Options (..))
import Game.Player (Player)
import Game.View (GameStateView)

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
  show (Receive _) = "Receive"
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

makeLenses ''Agent
