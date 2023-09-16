{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
module Game.Agent where

import Control.Concurrent (Chan)
import Control.Lens (makeLenses)
import Data.Text (Text)
import GHC.Generics
import Game.Choose (GameToInterfacePayload)
import Game.Options (Options (..))
import Game.Player (Player)
import Game.View (GameStateView)

data BEvent l cn r ph pl i
  = Receive (GameStateView l cn r ph)
  | Request (Options pl i)
  | AnnounceWinner [Player]
  | AnnounceEvent (Maybe Player) Text
  deriving (Generic)

extractReceive :: BEvent l cn r ph pl i -> Maybe (GameStateView l cn r ph)
extractReceive (Receive gsv) = Just gsv
extractReceive _ = Nothing

instance (Show pl, Show i) => Show (BEvent l cn r ph pl i) where
  show (Receive _) = "Receive"
  show (Request opts) = "Request (" ++ show opts ++ ")"
  -- show (Answer play) = "Answer (" ++ show play ++ ")"
  show (AnnounceWinner winners) = show (head winners) ++ " is the winner!"
  show (AnnounceEvent speaker announcement) = show speaker ++ " announces: " ++ show announcement

-- TODO: just make agents handle events

data Agent l cn r ph pl i m = Agent
  { playChooser :: GameStateView l cn r ph -> Options pl i -> m pl,
    stateHandler :: GameStateView l cn r ph -> m (),
    winnersHandler :: [Player] -> m (),
    announceHandler :: Maybe Player -> Text -> m (),
    fromGameChannel :: Chan (GameToInterfacePayload l cn r ph pl i),
    toGameChannel :: Chan pl
  }
  deriving (Generic)

makeLenses ''Agent
