module Game.Visibility where
import Game.Player (Player)
import GHC.Generics (Generic)


data VisibilityType = Invisible | Visible deriving (Eq, Ord, Show, Generic)

swapVis :: VisibilityType -> VisibilityType
swapVis Invisible = Visible
swapVis Visible = Invisible

runVis :: VisibilityType -> a -> Maybe a
runVis Invisible _ = Nothing
runVis Visible a = Just a

data VisData l cn ph = VisLocation l
            | VisCounter cn
            | VisTurn Player
            | VisCurrentPhase
            deriving (Eq, Ord, Show, Generic)

newtype VisibilityMap l cn ph = VisibilityMap {canSee :: Player -> VisData l cn ph -> VisibilityType}
    deriving Generic

allVisible :: VisibilityMap l c ph
allVisible = VisibilityMap (\_ _ -> Visible)

makeVisible :: (Eq l, Eq c, Eq ph) => VisibilityMap l c ph -> Player -> VisData l c ph -> VisibilityMap l c ph
makeVisible (VisibilityMap vis) player lc = VisibilityMap (makeVisible' vis player lc) where
    makeVisible' :: Eq lc => (Player -> lc -> VisibilityType) -> Player -> lc -> (Player -> lc -> VisibilityType)
    makeVisible' vis' p lc p' l' = if p == p' && lc == l' then Visible else vis' p' l'

makeInvisible :: (Eq l, Eq c, Eq ph) => VisibilityMap l c ph -> Player -> VisData l c ph -> VisibilityMap l c ph
makeInvisible (VisibilityMap vis) player lc = VisibilityMap (makeInvisible' vis player lc) where
    makeInvisible' :: Eq lc => (Player -> lc -> VisibilityType) -> Player -> lc -> (Player -> lc -> VisibilityType)
    makeInvisible' vis' p lc p' l' = if p == p' && lc == l' then Invisible else vis' p' l'

