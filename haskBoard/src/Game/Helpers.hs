{-# LANGUAGE RankNTypes #-}
module Game.Helpers
    where
import Game.Player
import Game.GameNode
import Count
import Game.Location (howMany', howManyF, has', LocationShape, Counter, listAllShapeF)
import Game.Monad (  GameEff(..), askEff, hoistGameEff, ViewerType (..))
import Util (graphM)
import Game.Visibility (VisibilityType(..), VisData (..), VisibilityMap (..), runVis)
import Control.Lens (view, mapping, Getter, to, (^.), mapped, (^..), (^?), pre)
import Control.Monad.Trans (MonadTrans(..))
import FinitaryMap (ftAt)
import Data.Set (Set)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Morph (hoist)
import Game.GameState

viewPlayers :: GameEff l cn r ph pl i (Set Player)
viewPlayers = view  #players . fst <$> askEff

viewLocation :: Eq l => l -> GameEff l cn r ph pl i (LocationShape r)
viewLocation l = do
    (g,viewer) <- askEff
    let VisibilityMap vis = g ^. #visibility
    let value = g ^.  #objects . #locations . ftAt l 
    case viewer of
      ViewAs p -> hoistGameEff $ runVis (vis p (VisLocation l))  value
      ViewFull -> hoistGameEff . Just $ value

viewCounter :: (Eq cn) => cn -> GameEff l cn r ph pl i Counter
viewCounter c = do
    (g,viewer) <- askEff
    let VisibilityMap vis = g ^. #visibility
    let value = g ^.  #objects . #counters . ftAt c 
    case viewer of
      ViewAs p -> hoistGameEff $ runVis (vis p (VisCounter c))  value
      ViewFull -> hoistGameEff . Just $ value

viewCounterBounds :: (Eq cn) => cn -> GameEff l cn r ph pl i (Cnt Int, Cnt Int)
viewCounterBounds cn = fmap (view #bounds) (viewCounter cn) 

viewCounterVal :: Eq cn => cn -> GameEff l cn r ph pl i (Cnt Int)
viewCounterVal cn = fmap (view #val) (viewCounter cn)

-- viewCounterValC :: Eq cn => cn -> GameStateViewC l cn r ph pl i -> Maybe (Cnt Int)
-- viewCounterValC cn = fmap (view #val) . view (#objectsViewC . #countersViewC . ftAt cn)

viewCurrentPhase :: GameEff l cn r ph pl i ph
viewCurrentPhase = view #currentPhase . fst <$> askEff

-- lenses 
lookLoc :: Eq l => Player -> l -> Getter (GameState l cn r ph pl i) (Maybe (LocationShape r))
lookLoc p l = to $ \gs -> let
            VisibilityMap vis = gs ^. #visibility
            in
            runVis  (vis p (VisLocation l)) (gs ^. #objects . #locations . ftAt l)

lookCounterVal ::  Eq cn => Player -> cn -> Getter (GameState l cn r ph pl i) (Maybe (Cnt Int))
lookCounterVal p c = to $ \gs -> let
            VisibilityMap vis = gs ^. #visibility
            in
            runVis (vis p (VisCounter c)) (gs ^. #objects . #counters . ftAt c . #val)

lookCounterBounds ::  Eq cn => Player -> cn -> Getter (GameState l cn r ph pl i) (Maybe (Cnt Int, Cnt Int))
lookCounterBounds p c = to $ \gs -> let
            VisibilityMap vis = gs ^. #visibility
            in
            runVis (vis p (VisCounter c)) (gs ^. #objects . #counters . ftAt c . #bounds)

lookCurrentPhase :: Player ->  Getter (GameState l cn r ph pl i) (Maybe ph)
lookCurrentPhase p = to $ \gs -> let
            VisibilityMap vis = gs ^. #visibility
            in
            runVis (vis p VisCurrentPhase) (gs ^. #currentPhase)

lookTurnOwner :: Player -> Getter (GameState l cn r ph pl i) (Maybe Player)
lookTurnOwner p = to $ \gs -> let
            VisibilityMap vis = gs ^. #visibility
            in
            runVis (vis p VisCurrentPhase) (gs ^. #currentTurn . #owner)


--- functions

-- -- listAllF :: Ord r => n -> Locations n r -> (r -> Bool) -> [r]
-- -- listAllF n locs = listAllShapeF (locs !!! n)

listAllFM :: (Ord r, Eq l) => l -> (r -> Bool) -> GameEff l cn r ph pl i [r]
listAllFM l filt = flip listAllShapeF filt <$> viewLocation l


findResourceWithin' ::  (Ord r, Eq l) =>  r -> [l] -> GameEff l cn r ph pl i [l]
findResourceWithin' r locationNames = do
  shapes <- traverse (graphM viewLocation) locationNames
  let whoHasIt = filter ((`has'` r) . snd) shapes
  return (fmap fst whoHasIt)

mkMoveNode :: Player -> GameAction l cn r ph -> GameNode l cn r ph pl i
mkMoveNode p act = GameNode (Left act) (Just p)

howManyAt :: (Ord r, Eq l) => l -> r -> GameEff l cn r ph pl i (Cnt Int)
howManyAt l r = flip howMany' r <$> viewLocation l

has ::  (Ord r, Eq l) =>  l -> r -> GameEff l cn r ph pl i Bool
has l r = (> 0) <$> howManyAt l r

doesNotHave ::  (Ord r, Eq l) =>  l -> r -> GameEff l cn r ph pl i Bool
doesNotHave l r = not <$> has l r

-- hasAny :: (Ord r, Eq l) => l -> (r -> Bool) -> GameView l cn r ph pl i Bool
-- hasAny l filt = (> 0) . flip howManyF filt <$> viewLocation l

-- transferAll :: (Ord r, Eq l) => l -> l -> r -> GameView l cn r ph pl i [GameNode l cn r ph pl i]
-- transferAll source target res = do
--     numRes <- howManyAt source res
--     case numRes of
--       Cnt i -> return . fmap mkActionNode $ replicate i (MkTransfer source target res)
--       _ -> return . fmap mkActionNode $ repeat (MkTransfer source target res)

(<+) :: Enum a => a -> Int -> a
a <+ i
  | i > 0 = succ a <+ (i - 1)
  | i < 0 = pred a <+ (i + 1)
  | otherwise = a


