{-# LANGUAGE TypeFamilies #-}

module Game.Choose where

import Data.Map (Map)
import Data.Text
import Effectful
import Effectful.Dispatch.Dynamic
import GHC.Generics (Generic)
import Game.GameState
import Game.Options (Options)
import Game.Player
import Game.View (GameStateView)

data Interface l cn r ph pl :: Effect where
  Choose :: GameState l cn r ph pl -> Options pl -> (Interface l cn r ph pl) m pl
  Update :: GameState l cn r ph pl -> Map Player Int -> (Interface l cn r ph pl) m ()
  AnnounceWinners :: [Player] -> (Interface l cn r ph pl) m ()
  Announce :: (Maybe Player) -> Text -> Interface l cn r ph pl m ()

type instance DispatchOf (Interface l cn r ph pl) = 'Dynamic

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
-- TODO: should this include GameState???
choose :: (Interface l cn r ph pl :> es) => GameState l cn r ph pl -> Options pl -> Eff es pl
choose gs cs = send (Choose gs cs)

choose' :: (Interface l cn r ph pl :> es, GameInteract l cn r ph pl :> es) => Options pl -> Eff es pl
choose' cs = getGameState >>= (`choose` cs)

update :: (Interface l cn r ph pl :> es) => GameState l cn r ph pl -> Map Player Int -> Eff es ()
update gs scores = send (Update gs scores)

announceWinners :: (Interface l cn r ph pl :> es) => [Player] -> Eff es ()
announceWinners winners = send (AnnounceWinners winners)

announce :: (Interface l cn r ph pl :> es) => Maybe Player -> Text -> Eff es ()
announce speaker announcement = send (Announce speaker announcement)
