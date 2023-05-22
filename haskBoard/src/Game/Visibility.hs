module Game.Visibility where
import Game.Player (Player)
import GHC.Generics (Generic)


data VisibilityType = Invisible | Visible deriving (Eq, Ord, Show, Generic)

runVis :: VisibilityType -> a -> Maybe a
runVis Invisible _ = Nothing
runVis Visible a = Just a


newtype VisibilityMap l c = VisibilityMap {canSee :: Player -> Either l c -> VisibilityType}
    deriving Generic

allVisible :: VisibilityMap l c
allVisible = VisibilityMap (\_ _ -> Visible)

makeVisible :: (Eq l, Eq c) => VisibilityMap l c -> Player -> Either l c -> VisibilityMap l c
makeVisible (VisibilityMap vis) player lc = VisibilityMap (makeVisible' vis player lc) where
    makeVisible' :: Eq lc => (Player -> lc -> VisibilityType) -> Player -> lc -> (Player -> lc -> VisibilityType)
    makeVisible' vis' p lc p' l' = if p == p' && lc == l' then Visible else vis' p' l'

makeInvisible :: (Eq l, Eq c) => VisibilityMap l c -> Player -> Either l c -> VisibilityMap l c
makeInvisible (VisibilityMap vis) player lc = VisibilityMap (makeInvisible' vis player lc) where
    makeInvisible' :: Eq lc => (Player -> lc -> VisibilityType) -> Player -> lc -> (Player -> lc -> VisibilityType)
    makeInvisible' vis' p lc p' l' = if p == p' && lc == l' then Invisible else vis' p' l'

