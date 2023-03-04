{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
module Game.Objects
    where
import GHC.Generics (Generic)
import Location
import Control.Lens (makeFields)

-- All GameObject querying should happen here!

data GameObjects n cn r = GameObjects {
    locations :: Locations n r,
    counters :: Counters cn} deriving (Generic, Show)

makeFields ''GameObjects

counterVal :: cn -> GameObjects n cn r -> (Cnt Int, Cnt Int)
counterVal counter gos = view (#counters . ftAt cn . #val)

