{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Game.Player where

import GHC.Generics (Generic)
import GHC.Word (Word8)
import Data.Finitary (Finitary)

newtype Player = Player {num :: Word8} deriving (Eq, Ord, Show, Generic, Read, Bounded, Finitary)

mkPlayers :: Int -> [Player]
mkPlayers i = Player <$> [0..fromIntegral i]
