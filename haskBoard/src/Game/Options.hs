{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
module Game.Options where
import GHC.Generics (Generic)
import Data.List.NonEmpty (NonEmpty)
import Control.Lens (makeFields)
import Data.Map (Map)

-- A move may have lots of illegality.
-- Concatenate where possible.

data Legality illegal = Legal | Illegal [illegal] deriving (Eq, Ord, Show, Generic, Functor)

instance Semigroup (Legality i) where
    Legal <> x = x
    x <> Legal = x
    Illegal x <> Illegal y = Illegal (x <> y)


instance Monoid (Legality i) where
    mempty = Legal

data Options pl i = Options {legal :: NonEmpty pl,
                             illegal :: Map pl (Legality i)
                            } deriving (Eq, Ord, Show, Generic)


makeFields ''Options
