{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Game.Agent where

import Control.Concurrent (Chan)
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON, ToJSONKey)
import Data.Aeson.Text (encodeToLazyText)
import Data.Finitary (Finitary)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import GHC.Generics
import Game.Choose (GameToInterfacePayload)
import Game.Options (Options (..))
import Game.Player (Player)
import Game.View (GameStateView)

data BEvent l cn r ph pl
  = Receive (GameStateView l cn r ph)
  | Request (Options pl)
  | AnnounceWinner [Player]
  | -- TODO: why does this have Player argument? Probably shouldn't
    AnnounceEvent (Maybe Player) Text
  deriving (Generic)

extractReceive :: BEvent l cn r ph pl -> Maybe (GameStateView l cn r ph)
extractReceive (Receive gsv) = Just gsv
extractReceive _ = Nothing

instance (Show pl) => Show (BEvent l cn r ph pl) where
  show (Receive _) = "Receive"
  show (Request opts) = "Request (" ++ show opts ++ ")"
  -- show (Answer play) = "Answer (" ++ show play ++ ")"
  show (AnnounceWinner winners) = show (head winners) ++ " is the winner!"
  show (AnnounceEvent speaker announcement) = show speaker ++ " announces: " ++ show announcement

-- TODO: just make agents handle events

data Agent l cn r ph pl m = Agent
  { playChooser :: GameStateView l cn r ph -> Options pl -> m pl,
    stateHandler :: GameStateView l cn r ph -> m (),
    winnersHandler :: [Player] -> m (),
    announceHandler :: Maybe Player -> Text -> m (),
    fromGameChannel :: Chan (GameToInterfacePayload l cn r ph pl),
    toGameChannel :: Chan pl
  }
  deriving (Generic)

makeLenses ''Agent

data BEventMessage
  = ReceiveMessage Text
  | RequestMessage Text
  | AnnounceWinnerMessage Text
  | AnnounceEventMessage Text
  deriving (Eq, Ord, Show)

-- mkEventMessage :: (ToJSON pl, ToJSON cn, ToJSON r, ToJSON l, ToJSON ph, ToJSONKey cn, ToJSONKey l, ToJSONKey r, Ord cn, Ord l, Finitary cn, Finitary l) => BEvent l cn r ph pl -> BEventMessage
-- mkEventMessage (Receive gsv) = ReceiveMessage (toStrict . encodeToLazyText $ gsv)
-- mkEventMessage (Request opts) = RequestMessage (toStrict . encodeToLazyText $ opts)
-- mkEventMessage (AnnounceWinner ps) = AnnounceWinnerMessage (toStrict . encodeToLazyText $ ps)
-- mkEventMessage (AnnounceEvent mp txt) = AnnounceEventMessage txt
