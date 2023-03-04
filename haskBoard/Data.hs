{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
module Game.Objects
    where
import GHC.Generics (Generic)
import Location
import Control.Lens (makeFields)
import Count (Cnt)
import FinitaryMap (ftAt)
import Control.Lens.Combinators (view)

-- All GameObject querying should happen here!

data GameObjects n cn r = GameObjects {
    locations :: Locations n r,
    counters :: Counters cn} deriving (Generic, Show)

makeFields ''GameObjects

counterVal :: Eq cn => cn -> GameObjects n cn r -> Cnt Int 
counterVal counter = view (#counters . ftAt counter. #val)


