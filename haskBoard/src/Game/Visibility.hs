module Game.Visibility where
import Game.Player (Player)
import GHC.Generics (Generic)


data VisibilityType = Invisible | Visible deriving (Eq, Ord, Show, Generic)

newtype VisibilityMap l c = VisibilityMap {canSee :: Player -> Either l c -> VisibilityType}

allVisible :: VisibilityMap l c
allVisible = VisibilityMap (\_ _ -> Visible)

makeVisible :: (Eq l, Eq c) => VisibilityMap l c -> Player -> Either l c -> VisibilityMap l c
makeVisible visibility player lc = VisibilityMap (makeVisible' (canSee visibility) player lc) where
    makeVisible' :: Eq lc => (Player -> lc -> VisibilityType) -> Player -> lc -> (Player -> lc -> VisibilityType)
    makeVisible' vis p lc p' l' = if p == p' && lc == l' then Visible else vis p' l'

makeInvisible :: (Eq l, Eq c) => VisibilityMap l c -> Player -> Either l c -> VisibilityMap l c
makeInvisible visibility player lc = VisibilityMap (makeInvisible' (canSee visibility) player lc) where
    makeInvisible' :: Eq lc => (Player -> lc -> VisibilityType) -> Player -> lc -> (Player -> lc -> VisibilityType)
    makeInvisible' vis p lc p' l' = if p == p' && lc == l' then Invisible else vis p' l'


-- type VisibilityE l = State (VisibilityMap l)
-- look :: VisibilityE l :> es => Player -> l -> Eff es VisibilityType
-- look p l = do
--     lookAt <- gets canSee
--     return (p `lookAt` l)


-- need to figure out interface first
