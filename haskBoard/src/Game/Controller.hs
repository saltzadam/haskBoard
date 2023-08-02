{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Game.Controller where

import Control.Concurrent (Chan, readChan, writeChan)
import Control.Exception
import Control.Lens (at, makeLenses, to, (^.))
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as M
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import GHC.Generics (Generic)
import Game.Agent (Agent)
import Game.Choose (GameToInterfacePayload, Interface (..))
import Game.GameState (GameState)
import Game.Monad (LookerType (..))
import Game.Options (Options)
import Game.Player (Player)
import Game.View (GameStateView, viewGameStateAs)

-- controller should distribute Interface events and collect results
