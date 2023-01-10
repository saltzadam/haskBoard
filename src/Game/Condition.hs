{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
    {-# LANGUAGE ScopedTypeVariables #-}

module Game.Condition where

import Count
import Game
import Control.Lens
import Util
import Location
import Control.Monad.State.Lazy (get)
import FinitaryMap (ftAt)
-- import Data.Text.Lazy (Text, pack)
-- import Formatting
-- import Formatting.ShortFormatters (sh)
cEmpty :: Condition l cn r ph pl t tn [a]
cEmpty = pure []

cIn :: Eq a => Condition l cn r ph pl t tn (a -> [a] -> Bool)
cIn = pure elem

cTrue :: Condition l cn r ph pl t tn Bool
cTrue = pure True

cCounterVal :: (Ord cn) => cn -> Condition l cn r ph pl t tn (Cnt Int)
cCounterVal cn = Condition $ view (#objects . #counters . ftAt cn . #val) <$> get

cHas :: (Ord r, Ord l) => Game l cn r ph pl t tn -> r -> l -> Bool
cHas g res loc = (`has'` res) . view (#objects . #locations . ftAt loc) $ g

