module Options where

data Legality illegal = Legal | Illegal illegal deriving (Eq, Ord, Show, Generic)

instance Semigroup (Legality i) where
    Legal <> x = x
    x <> Legal = x
    x <> _ = x

instance Monoid (Legality i) where
    mempty = Legal

data Options pl i = Options {legal :: NonEmpty pl,
                             illegal :: [(pl, Legality i)]
                            } deriving (Eq, Ord, Show, Generic)


makeFields ''Options
