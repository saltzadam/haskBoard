{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Interface.Protocol
  ( InitMsg (..),
    StepMsg (..),
    InMsg (..),
    ActionSource (..),
    buildInitMsg,
    encodeGameObjectsObs,
    addScoresToObs,
  )
where

import Control.Lens ((^.))
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withObject, (.:))
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import Data.Finitary (inhabitants)
import Data.Generics.Labels ()
import Data.Map (Map)
import qualified Data.Map as M
import Data.Proxy (Proxy (..))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import FinitaryMap ((!!!))
import GHC.Generics (Generic)
import Game.Constraints (GameCounter, GameLocation, GamePlay, GameResource)
import Game.GameState (GameRules, GameState)
import Game.Location
  ( GymSpace (..),
    NormHint (..),
    encodeCounterObs,
    encodeLocationObs,
    inventoryTotals,
  )
import Game.Options (actionSpaceSize)
import Game.Player (Player (..))
import Game.View (GameObjectsView (..), gameObjectsViewSpace, viewGameStateAs')

-- ---- Message types ----

data ActionSource = Hint | Random | Agent | Human
  deriving (Show, Generic, ToJSON)

data InitMsg = InitMsg
  { agents :: [Int],
    observationSpaces :: M.Map Int GymSpace,
    actionSpace :: GymSpace
  }
  deriving (Generic, ToJSON)

data StepMsg = StepMsg
  { msgType :: Text,
    agent :: Int,
    observation :: Value,
    legalActions :: [Int],
    reward :: Float,
    terminated :: Bool,
    truncated :: Bool,
    hintAction :: Maybe Int,
    actionSource :: ActionSource
  }
  deriving (Generic, ToJSON)

data InMsg
  = ActionMsg Int
  | ResetMsg
  deriving (Show)

instance FromJSON InMsg where
  parseJSON = withObject "InMsg" $ \o -> do
    t <- o .: "type" :: Parser Text
    case t of
      "action" -> ActionMsg <$> o .: "action"
      "reset"  -> pure ResetMsg
      _        -> fail $ "Unknown message type: " ++ T.unpack t

-- ---- Observation encoding ----

encodeGameObjectsObs
  :: forall l cn r. (GameLocation l, GameCounter cn, GameResource r)
  => Map r Int -> GameObjectsView l cn r -> Value
encodeGameObjectsObs totals (GameObjectsView locsView cnsView) =
  toJSON $ M.fromList $
    [(T.pack (show l), encodeLocationObs totals (Just loc)) | l <- inhabitants @l, Just loc <- [locsView !!! l]]
    ++ [(T.pack (show cn), encodeCounterObs (Just c)) | cn <- inhabitants @cn, Just c <- [cnsView !!! cn]]

-- | Merge a "scores" array into an existing observation Value (a JSON object).
-- When isPublic=True, the array contains all players' scores (ascending player order).
-- When isPublic=False, it contains only thisPlayer's score (length 1).
addScoresToObs :: M.Map Player Int -> Bool -> Player -> Value -> Value
addScoresToObs allScores isPublic thisPlayer (Object km) =
  let scoreList = if isPublic
                  then map snd (M.toAscList allScores)
                  else case M.lookup thisPlayer allScores of
                         Nothing -> []
                         Just s  -> [s]
  in Object (KM.insert (fromText "scores") (toJSON scoreList) km)
addScoresToObs _ _ _ v = v

-- ---- Initialization ----

buildInitMsg
  :: forall l cn r ph pl.
     (GameLocation l, GameCounter cn, GameResource r, GamePlay pl)
  => GameState l cn r ph pl -> GameRules l cn r ph pl -> InitMsg
buildInitMsg gs gr = InitMsg agentNums obsSpaces actSpace
  where
    toAgentNum (Player pnum) = fromEnum pnum
    players    = S.toList (gs ^. #players)
    agentNums  = map toAgentNum players
    totals     = inventoryTotals (gs ^. (#objects . #locations))
    (lo, hi)   = gr ^. #scoreBounds
    isPublic   = gr ^. #scorePublic
    numPlayers = length players
    scoreCount _ = if isPublic then numPlayers else 1
    scoreSpace p  = GymBox (fromIntegral lo) (fromIntegral hi) [scoreCount p] MinMax
    addScores' p (GymDict pairs) = GymDict (pairs ++ [("scores", scoreSpace p)])
    addScores' _ other = other
    obsSpaces = M.fromList
      [ (toAgentNum p, addScores' p (gameObjectsViewSpace totals (viewGameStateAs' gs p ^. #objectsView)))
      | p <- players ]
    actSpace  = GymDiscrete (actionSpaceSize (Proxy @pl)) NoNorm
