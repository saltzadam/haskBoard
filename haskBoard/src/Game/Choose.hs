{-# LANGUAGE TypeFamilies #-}

module Game.Choose where

import Control.Monad.Free (liftF)
import Effectful
import Effectful.Dispatch.Dynamic
import GHC.Generics (Generic)
import Game.GameState
import Game.Options (Options)
import Game.Player
import Game.Rules (GameRule, GameRuleF (..))
import Game.View (GameStateView)

data Interface l cn r ph pl i :: Effect where
  Choose :: GameState l cn r ph pl i -> Options pl i -> (Interface l cn r ph pl i) m pl
  Update :: GameState l cn r ph pl i -> (Interface l cn r ph pl i) m ()
  AnnounceWinners :: [Player] -> (Interface l cn r ph pl i) m ()

type instance DispatchOf (Interface l cn r ph pl i) = 'Dynamic

data GameToInterfacePayload l cn r ph pl i
  = SendState (GameStateView l cn r ph)
  | SendOptions (GameStateView l cn r ph) (Options pl i)
  | SendWinners [Player]
  deriving (Generic)

-- TODO: should this include GameState???
choose :: (Interface l cn r ph pl i :> es) => GameState l cn r ph pl i -> Options pl i -> Eff es pl
choose gs cs = send (Choose gs cs)

choose' :: (Interface l cn r ph pl i :> es, GameInteract l cn r ph pl i :> es) => Options pl i -> Eff es pl
choose' cs = getGameState >>= (`choose` cs)

update :: (Interface l cn r ph pl i :> es) => GameState l cn r ph pl i -> Eff es ()
update gsvc = send (Update gsvc)

announceWinners :: (Interface l cn r ph pl i :> es) => [Player] -> Eff es ()
announceWinners winners = send (AnnounceWinners winners)

mkChoice :: Options pl i -> GameRule l cn r ph pl i pl
mkChoice opts = liftF (MakeChoice opts id)
