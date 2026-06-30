{-# LANGUAGE ConstraintKinds #-}

module Game.Constraints where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Finitary (Finitary)

-- | Everything a location-name type must satisfy to work across the system.
type GameLocation l =
  (Finitary l, Eq l, Ord l, Show l,
   ToJSON l, ToJSONKey l, FromJSON l, FromJSONKey l)

-- | Everything a counter-name type must satisfy.
type GameCounter cn =
  (Finitary cn, Eq cn, Ord cn, Show cn,
   ToJSON cn, ToJSONKey cn, FromJSON cn, FromJSONKey cn)

-- | Everything a resource type must satisfy.
type GameResource r =
  (Finitary r, Eq r, Ord r, Show r, ToJSON r, FromJSON r, FromJSONKey r, ToJSONKey r)

-- | Everything a phase-name type must satisfy.
-- ph is NOT required to be Finitary — phases often carry data like Player.
type GamePhase ph =
  (Eq ph, Ord ph, Show ph, ToJSON ph, FromJSON ph)

-- | Everything a play type must satisfy.
type GamePlay pl =
  (Finitary pl, Eq pl, Ord pl, Show pl, ToJSON pl, FromJSON pl)
