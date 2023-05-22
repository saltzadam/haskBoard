module Game.Helpers
    where
import Game.Player
import Game.GameNode
import Count
import Game.Location (howMany', howManyF, has', LocationShape, Counter, listAllShapeF)
import Game.Monad (GameView(..), askMM, runVis)
import Util (graphM)
import Game.Visibility (VisibilityType(..))
import Control.Lens (view, mapping)
import Control.Monad.Trans (MonadTrans(..))
import FinitaryMap (ftAt)
import Data.Set (Set)
import Game.View (GameStateViewC)

viewPlayers :: GameView l cn r ph pl i (Set Player)
viewPlayers = view  #players . fst <$> askMM

viewLocation :: Eq l => l -> GameView l cn r ph pl i (LocationShape r)
viewLocation l = do
    (g,viewer) <- askMM
    let vis = view #visibility g
    case runVis vis viewer (Left l) of
      Invisible -> GameView (lift Nothing)
      Visible -> view ( #objects . #locations . ftAt l) . fst <$> askMM

viewCounter :: (Eq cn) => cn -> GameView l cn r ph pl i Counter
viewCounter c = do
    (g,viewer) <- askMM
    let vis = view #visibility g
    case runVis vis viewer (Right c) of
      Invisible -> GameView (lift Nothing)
      Visible -> view ( #objects . #counters . ftAt c) . fst <$> askMM

viewCounterBounds :: (Eq cn) => cn -> GameView l cn r ph pl i (Cnt Int, Cnt Int)
viewCounterBounds cn = view #bounds <$> viewCounter cn

viewCounterVal :: Eq cn => cn -> GameView l cn r ph pl i (Cnt Int)
viewCounterVal cn = view #val <$> viewCounter cn

viewCounterValC :: Eq cn => cn -> GameStateViewC l cn r ph pl i -> Maybe (Cnt Int)
viewCounterValC cn = fmap (view #val) . view (#objectsViewC . #countersViewC . ftAt cn)

viewCurrentPhase :: GameView l cn r ph pl i ph
viewCurrentPhase = view #currentPhase . fst <$> askMM

-- listAllF :: Ord r => n -> Locations n r -> (r -> Bool) -> [r]
-- listAllF n locs = listAllShapeF (locs !!! n)

listAllFM :: (Ord r, Eq l) => l -> (r -> Bool) -> GameView l cn r ph pl i [r]
listAllFM l filt = flip listAllShapeF filt <$> viewLocation l


findResourceWithin' ::  (Ord r, Eq l) =>  r -> [l] -> GameView l cn r ph pl i [l]
findResourceWithin' r locationNames = do
  shapes <- traverse (graphM viewLocation) locationNames
  let whoHasIt = filter ((`has'` r) . snd) shapes
  return (fmap fst whoHasIt)

mkMoveNode :: Player -> GameAction l cn r ph -> GameNode l cn r ph pl i
mkMoveNode p act = GameNode (Left act) (Just p)

howManyAt :: (Ord r, Eq l) => l -> r -> GameView l cn r ph pl i (Cnt Int)
howManyAt l r = flip howMany' r <$> viewLocation l

has ::  (Ord r, Eq l) =>  l -> r -> GameView l cn r ph pl i Bool
has l r = (> 0) <$> howManyAt l r

doesNotHave ::  (Ord r, Eq l) =>  l -> r -> GameView l cn r ph pl i Bool
doesNotHave l r = not <$> has l r

hasAny :: (Ord r, Eq l) => l -> (r -> Bool) -> GameView l cn r ph pl i Bool
hasAny l filt = (> 0) . flip howManyF filt <$> viewLocation l

transferAll :: (Ord r, Eq l) => l -> l -> r -> GameView l cn r ph pl i [GameNode l cn r ph pl i]
transferAll source target res = do
    numRes <- howManyAt source res
    case numRes of
      Cnt i -> return . fmap mkActionNode $ replicate i (MkTransfer source target res)
      _ -> return . fmap mkActionNode $ repeat (MkTransfer source target res)

(<+) :: Enum a => a -> Int -> a
a <+ i
  | i > 0 = succ a <+ (i - 1)
  | i < 0 = pred a <+ (i + 1)
  | otherwise = a


