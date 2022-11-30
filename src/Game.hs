{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Game
    where
import Data.Set (Set)

import Count
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (Sum)
import GHC.Base (Type)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
-- import Data.Finitary
import Control.Lens (makeLenses, view, at) 

-- given some locations and resources...
newtype Player = Player String deriving (Eq, Show, Ord)


data Resource = Card Int | Gem Bool deriving (Eq, Ord, Show)

-- data GameObjects resources locations =
--     GameObjects {
--         players :: Set Player,
--         inventory :: locations -> Location locations
--                 }

-- data GameObjects resources locations visibility = 
--     GameObjects {
--         players :: Set Player,
--         inventory :: locations -> Stack resources,
--         visibility :: locations -> Player -> visibility}



-- data Move resources locations = Move (Stack resources) locations locations
-- -- -- stuff source target

-- data Condition r l v = And (Condition r l v) (Condition r l v)
--                    | Or (Condition r l v) (Condition r l v)
--                    | Not (Condition r l v) (Condition r l v)
--                    | If (Condition r l v) (Condition r l v) (Condition r l v) -- if then else
--                    | Has l (Stack r)
--                    | HasAtLeast l (Stack r)
--                    | Turn Int
--                    | TurnAtLeast Int

-- type EvalCondition r l v c = GameObjects r l v -> Condition r l v -> c

-- ppCondition :: EvalCondition r l String
-- ppCondition = undefined
-- evalCondition :: EvalCondition r l Bool
-- evalCondition = undefined
-- validateCondition :: EvalCondition r l Bool -- dunno
-- validateCondition = undefined

-- data Play r l v = Play Player (Condition r l v) (GameObjects r l v -> [Move r l ])

-- move res    source source = id
-- move mempty source target = id
-- NO: move res target nextTarget . move res source target
--  != move res source nextTarget
--  Need to know the result of the first move
--
-- (inventory source <> inventory target) = (move res source target) (inventory source) <> (move res source target) (inventory target)

-- cRemove :: Counter -> Counter -> (Counter, Counter)
-- (Counter i) `cRemove` (Counter j) = (Counter (max 0 (i - j)), Counter (min i j))

-- remove :: Resource -> Resource -> (Resource, Resource)
-- remove r r' = let
--                 f :: ResourceType -> (Counter, Counter)
--                 f rtype = r rtype `cRemove` r' rtype
--               in 
--                 (fst . f, snd . f)

-- should be move :: inventory -> Move -> inventory
-- move :: Inventory -> Location -> Resource -> Location -> Inventory
-- move inventory source stuff target loc = 
--     let 
--         (newSource, moved) = inventory source `remove` stuff
--      in if loc == source then newSource
--         else if loc == target
--              then inventory target <> moved
--              else inventory loc



{- 
Games also have control flow. The game rules will dictate everything that happens until a player
needs to make a choice. This choice is called a Play. The game rules determine
which Plays are legal. The game can enumerate the legal plays from any position.

-}
