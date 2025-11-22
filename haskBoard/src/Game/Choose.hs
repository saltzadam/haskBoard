{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module Game.Choose where

import Control.Monad.Free (liftF)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import Effectful
import Effectful.Dispatch.Dynamic
import GHC.Generics (Generic)
import Game.GameAction (GameAction)
import Game.GameState
import Game.Options (Options)
import Game.Player
import Game.Rules (GameRule, GameRuleF (..))
import Game.View (GameStateView)

data Interface l cn r ph pl :: Effect where
  Choose :: GameState l cn r ph pl -> Options pl -> (Interface l cn r ph pl) m pl
  Update :: GameState l cn r ph pl -> (Interface l cn r ph pl) m ()
  AnnounceWinners :: [Player] -> (Interface l cn r ph pl) m ()
  Announce :: (Maybe Player) -> Text -> Interface l cn r ph pl m ()

type instance DispatchOf (Interface l cn r ph pl) = 'Dynamic

data GameToInterfacePayload l cn r ph pl
  = SendState (GameStateView l cn r ph)
  | SendOptions (GameStateView l cn r ph) (Options pl)
  | SendWinners [Player]
  | SendAnnouncement (Maybe Player) Text
  deriving (Generic, FromJSON, ToJSON)

-- TODO: should this include GameState???
choose :: (Interface l cn r ph pl :> es) => GameState l cn r ph pl -> Options pl -> Eff es pl
choose gs cs = send (Choose gs cs)

choose' :: (Interface l cn r ph pl :> es, GameInteract l cn r ph pl :> es) => Options pl -> Eff es pl
choose' cs = getGameState >>= (`choose` cs)

update :: (Interface l cn r ph pl :> es) => GameState l cn r ph pl -> Eff es ()
update gsvc = send (Update gsvc)

announceWinners :: (Interface l cn r ph pl :> es) => [Player] -> Eff es ()
announceWinners winners = send (AnnounceWinners winners)

announce :: (Interface l cn r ph pl :> es) => Maybe Player -> Text -> Eff es ()
announce speaker announcement = send (Announce speaker announcement)

mkChoice :: Options pl -> GameRule l cn r ph pl pl
mkChoice opts = liftF (MakeChoice opts id)
