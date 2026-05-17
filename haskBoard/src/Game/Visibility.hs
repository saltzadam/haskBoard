{-# LANGUAGE DeriveAnyClass #-}

module Game.Visibility where

import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey)
import Game.Constraints (GameCounter, GameLocation)
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

data VisData l cn
  = VisLocation l
  | VisCounter cn
  | VisTurn Player
  | VisCurrentPhase
  deriving (Eq, Ord, Show, Generic, Finitary, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

newtype VisibilityMap l cn = VisibilityMap {canSee :: Player -> VisData l cn -> VisibilityType}
  deriving (Generic)

instance (GameLocation l, GameCounter cn) => ToJSON (VisibilityMap l cn) where
  toJSON = toJSON . reifyVisibilityMap

instance (GameLocation l, GameCounter cn) => FromJSON (VisibilityMap l cn) where
  parseJSON = fmap unreifyVisibilityMap . parseJSON

reifyVisibilityMap :: (Finitary l, Finitary cn) => VisibilityMap l cn -> Map Player (Map (VisData l cn) VisibilityType)
reifyVisibilityMap (VisibilityMap m) = M.fromAscList [(p, reifyFn (FTMap $ m p)) | p <- inhabitants :: [Player]]

unreifyVisibilityMap' :: (Ord l, Ord cn) => VisibilityType -> Map Player (Map (VisData l cn) VisibilityType) -> VisibilityMap l cn
unreifyVisibilityMap' defaultVis vm = VisibilityMap $ \player visData ->
  fromMaybe
    defaultVis
    ( do
        visDataToVisibility <- M.lookup player vm
        M.lookup visData visDataToVisibility
    )

unreifyVisibilityMap :: (Ord l, Ord cn) => Map Player (Map (VisData l cn) VisibilityType) -> VisibilityMap l cn
unreifyVisibilityMap = unreifyVisibilityMap' Invisible

allVisible :: VisibilityMap l cn
allVisible = VisibilityMap (\_ _ -> Visible)

allInvisible :: VisibilityMap l cn
allInvisible = VisibilityMap (\_ _ -> Invisible)

makeVisible :: (Eq l, Eq cn) => VisibilityMap l cn -> Player -> VisData l cn -> VisibilityMap l cn
makeVisible (VisibilityMap vis) player lc = VisibilityMap (makeVisible' vis player lc)
  where
    makeVisible' :: (Eq lc) => (Player -> lc -> VisibilityType) -> Player -> lc -> (Player -> lc -> VisibilityType)
    makeVisible' vis' p lc p' l' = if p == p' && lc == l' then Visible else vis' p' l'

makeInvisible :: (Eq l, Eq cn) => VisibilityMap l cn -> Player -> VisData l cn -> VisibilityMap l cn
makeInvisible (VisibilityMap vis) player lc = VisibilityMap (makeInvisible' vis player lc)
  where
    makeInvisible' :: (Eq lc) => (Player -> lc -> VisibilityType) -> Player -> lc -> (Player -> lc -> VisibilityType)
    makeInvisible' vis' p lc p' l' = if p == p' && lc == l' then Invisible else vis' p' l'

-- | Hide one piece of 'VisData' from all of the given players.
hideFromAll :: (Eq l, Eq cn) => [Player] -> VisData l cn -> VisibilityMap l cn -> VisibilityMap l cn
hideFromAll players vd vm = foldr (\p v -> makeInvisible v p vd) vm players

-- | Show one piece of 'VisData' to all of the given players.
showToAll :: (Eq l, Eq cn) => [Player] -> VisData l cn -> VisibilityMap l cn -> VisibilityMap l cn
showToAll players vd vm = foldr (\p v -> makeVisible v p vd) vm players

-- | Hide multiple 'VisData' from all of the given players.
hideManyFromAll :: (Eq l, Eq cn) => [Player] -> [VisData l cn] -> VisibilityMap l cn -> VisibilityMap l cn
hideManyFromAll players vds vm = foldr (\vd v -> hideFromAll players vd v) vm vds

-- | Show multiple 'VisData' to all of the given players.
showManyToAll :: (Eq l, Eq cn) => [Player] -> [VisData l cn] -> VisibilityMap l cn -> VisibilityMap l cn
showManyToAll players vds vm = foldr (\vd v -> showToAll players vd v) vm vds
