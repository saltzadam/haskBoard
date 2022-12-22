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
import Control.Monad.Trans.Reader (ask)
import Util
import Location
-- import Data.Text.Lazy (Text, pack)
-- import Formatting
-- import Formatting.ShortFormatters (sh)
cEmpty :: Condition l r ph pl [a]
cEmpty = pure []

cIn :: Eq a => Condition l r ph pl (a -> [a] -> Bool)
cIn = pure elem

cTrue :: Condition l r ph pl Bool
cTrue = pure True

cCounterVal :: Ord l => l -> Condition l r ph pl (Cnt Int)
cCounterVal l = Condition $ maybe 0 (view #val) . preview (#objects . #counters . ix l) <$> ask



cHas :: (Ord r, Ord l) => Game l r ph pl -> r -> l -> Bool
cHas g res loc = maybeToBool . fmap (`has'` res) . preview (#objects . #locations . at loc . non Dummy) $ g
