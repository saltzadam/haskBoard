module Game.Choose where

import Data.Map (Map)
import Data.Text
import GHC.Generics (Generic)
import Game.GameState
import Game.Options (Options)
import Game.Player
import Game.View (GameStateView)

-- | The engine's connection to the players. 'Interface.Controller.controllerInterface'
-- builds one from a 'GameController'. Tests can write one directly.
data Interface l cn r ph pl = Interface
  { choose :: GameState l cn r ph pl -> Options pl -> IO pl,
    update :: GameState l cn r ph pl -> Map Player Int -> IO (),
    announceWinners :: [Player] -> IO (),
    announce :: Maybe Player -> Text -> IO ()
  }
  deriving (Generic)

data GameToInterfacePayload l cn r ph pl
  = SendState (GameStateView l cn r ph) (Map Player Int)
  | SendOptions (GameStateView l cn r ph) (Options pl)
  | SendWinners [Player]
  | SendAnnouncement (Maybe Player) Text
  deriving (Generic)

data PayloadTxt
  = SendStateTxt Text
  | SendOptionsTxt Text Text
  | SendWinnersTxt Text
  | SendAnnouncementTxt Text Text
  deriving (Eq, Ord, Show)

--
-- encodeStrict :: (ToJSON a) => a -> Text
-- encodeStrict = toStrict . encodeToLazyText . toJSON
--
-- mkPayloadTxt :: (Finitary l, Finitary cn, Ord l, Ord cn, ToJSONKey r, ToJSONKey l, ToJSONKey cn, ToJSON ph, ToJSON l, ToJSON r, ToJSON cn, ToJSON pl) => GameToInterfacePayload l cn r ph pl -> PayloadTxt
-- mkPayloadTxt (SendState gsv) = SendStateTxt (encodeStrict gsv)
-- mkPayloadTxt (SendOptions gsv opts) = SendOptionsTxt (encodeStrict gsv) (encodeStrict opts)
-- mkPayloadTxt (SendWinners ps) = SendWinnersTxt (encodeStrict ps)
-- mkPayloadTxt (SendAnnouncement ps msg) = SendAnnouncementTxt (encodeStrict ps) (encodeStrict msg)
--
