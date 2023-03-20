{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Game.Condition where

import Count
import GameE hiding (use)
import Control.Lens
import Location
import FinitaryMap (ftAt)
import Effectful.Reader.Static (ask)
import qualified Effectful.Reader.Static as R

-- cEmpty :: Condition l cn r ph pl i [a]
-- cEmpty = pure []

-- cIn :: Eq a => Condition l cn r ph pl i (a -> [a] -> Bool)
-- cIn = pure elem

-- cTrue :: Condition l cn r ph pl i Bool
-- cTrue = pure True

-- -- cCounterVal :: (Ord cn) => cn -> Condition l cn r ph pl i (Cnt Int)
-- -- cCounterVal cn = view (#objects . #counters . ftAt cn . #val) <$> ask

-- cHas :: (Ord r, Ord l) => GameState l cn r ph -> r -> l -> Bool
-- cHas g res loc = (`has'` res) . view (#objects . #locations . ftAt loc) $ g

-- -- sHas :: (Ord r, Ord l) => r -> l -> Condition l cn r ph pl i Bool
-- -- sHas res loc = do
-- --     locView <- R.asks (view (#objects . #locations . ftAt loc))
-- --     return (locView `has'` res)


