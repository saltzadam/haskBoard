{-# LANGUAGE DeriveAnyClass #-}

module Game.Visibility where

import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey)
import Data.Finitary (Finitary, inhabitants)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import FinitaryMap (FTMap (..), reifyFn)
import GHC.Generics (Generic)
import Game.Player (Player)

-- how many consumers of this would benefit from Maybe-ish typeclasses?
data LookerType = LookAs Player | LookFull deriving (Generic, Eq, Ord, Show)

data VisibilityType = Invisible | Visible deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

swapVis :: VisibilityType -> VisibilityType
swapVis Invisible = Visible
swapVis Visible = Invisible

runVis :: VisibilityType -> a -> Maybe a
runVis Invisible _ = Nothing
runVis Visible a = Just a

data VisData l cn ph
  = VisLocation l
  | VisCounter cn
  | VisTurn Player
  | VisCurrentPhase
  deriving (Eq, Ord, Show, Generic, Finitary, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

newtype VisibilityMap l cn ph = VisibilityMap {canSee :: Player -> VisData l cn ph -> VisibilityType}
  deriving (Generic)

instance (Finitary l, Finitary cn, ToJSON l, ToJSON cn) => ToJSON (VisibilityMap l cn ph) where
  toJSON = toJSON . reifyVisibilityMap

instance (Finitary l, Finitary cn, Ord l, Ord cn, FromJSON l, FromJSON cn) => FromJSON (VisibilityMap l cn ph) where
  parseJSON = fmap unreifyVisibilityMap . parseJSON

reifyVisibilityMap :: (Finitary l, Finitary cn) => VisibilityMap l cn ph -> Map Player (Map (VisData l cn ph) VisibilityType)
reifyVisibilityMap (VisibilityMap m) = M.fromAscList [(p, reifyFn (FTMap $ m p)) | p <- inhabitants :: [Player]]

unreifyVisibilityMap' :: (Ord l, Ord cn) => VisibilityType -> Map Player (Map (VisData l cn ph) VisibilityType) -> VisibilityMap l cn ph
unreifyVisibilityMap' defaultVis vm = VisibilityMap $ \player visData ->
  fromMaybe
    defaultVis
    ( do
        visDataToVisibility <- M.lookup player vm
        M.lookup visData visDataToVisibility
    )

unreifyVisibilityMap :: (Ord l, Ord cn) => Map Player (Map (VisData l cn ph) VisibilityType) -> VisibilityMap l cn ph
unreifyVisibilityMap = unreifyVisibilityMap' Invisible

allVisible :: VisibilityMap l c ph
allVisible = VisibilityMap (\_ _ -> Visible)

makeVisible :: (Eq l, Eq c, Eq ph) => VisibilityMap l c ph -> Player -> VisData l c ph -> VisibilityMap l c ph
makeVisible (VisibilityMap vis) player lc = VisibilityMap (makeVisible' vis player lc)
  where
    makeVisible' :: (Eq lc) => (Player -> lc -> VisibilityType) -> Player -> lc -> (Player -> lc -> VisibilityType)
    makeVisible' vis' p lc p' l' = if p == p' && lc == l' then Visible else vis' p' l'

makeInvisible :: (Eq l, Eq c, Eq ph) => VisibilityMap l c ph -> Player -> VisData l c ph -> VisibilityMap l c ph
makeInvisible (VisibilityMap vis) player lc = VisibilityMap (makeInvisible' vis player lc)
  where
    makeInvisible' :: (Eq lc) => (Player -> lc -> VisibilityType) -> Player -> lc -> (Player -> lc -> VisibilityType)
    makeInvisible' vis' p lc p' l' = if p == p' && lc == l' then Invisible else vis' p' l'
