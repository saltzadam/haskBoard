{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$" #-}
module Game.Helpers
    where
import Game.Player
import Game.GameNode
import Game.Location (howMany',has', LocationShape, Counter, listAllShapeF, inventory, peek', Locations)
import Game.Monad (  GameEff(..), askEff, hoistGameEff, LookerType(..))
import Util (graphM, graph, inhabitantsSet, invertNestedMaps, ifNullElse, concatNE, buildSafeNonempty, compose, kleisliCompose)
import Game.Visibility (VisData (..), VisibilityMap (..), runVis)
import Control.Lens (view, Getter, to, (^.))
import FinitaryMap (ftAt, (!!!))
import Data.Set (Set)
import Game.GameState
import Game.View (GameStateView)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Finitary
import qualified Data.Set as S
import Data.List (nub, delete)
import Game.Options
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (Semigroup(..))
import Control.Monad (filterM, join)
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Foldable as F

-- look for gameeff

lookPlayers :: GameEff l cn r ph pl i (Set Player)
lookPlayers = view #players . fst <$> askEff

lookLocation :: Eq l => l -> GameEff l cn r ph pl i (LocationShape r)
lookLocation l = do
    (g,looker) <- askEff
    let VisibilityMap vis = g ^. #visibility
    let value = g ^.  #objects . #locations . ftAt l
    case looker of
      LookAs p -> hoistGameEff $ runVis (vis p (VisLocation l))  value
      LookFull -> hoistGameEff . Just $ value

lookCounter :: (Eq cn) => cn -> GameEff l cn r ph pl i Counter
lookCounter c = do
    (g,looker) <- askEff
    let VisibilityMap vis = g ^. #visibility
    let value = g ^.  #objects . #counters . ftAt c
    case looker of
      LookAs p -> hoistGameEff $ runVis (vis p (VisCounter c))  value
      LookFull -> hoistGameEff . Just $ value

lookCounterBounds :: (Eq cn) => cn -> GameEff l cn r ph pl i (Int, Int)
lookCounterBounds cn = fmap (view #bounds) (lookCounter cn)

lookCounterVal :: Eq cn => cn -> GameEff l cn r ph pl i Int
lookCounterVal cn = fmap (view #val) (lookCounter cn)

lookCurrentPhase :: GameEff l cn r ph pl i ph
lookCurrentPhase = view #currentPhase . fst <$> askEff

-- view for GameView
viewLocation :: Eq l => GameStateView l cn r ph -> l -> Maybe (LocationShape r)
viewLocation gsv l = gsv ^. #objectsView . #locationsView . ftAt l

viewCounterVal :: Eq cn => GameStateView l cn r ph -> cn -> Maybe Int
viewCounterVal gsv cn = fmap (view #val) $ gsv ^. #objectsView . #countersView . ftAt cn

viewCurrentPlayer ::  GameStateView l cn r ph -> Player
viewCurrentPlayer gsv = gsv ^. #currPlayer

viewHowManyAt :: (Ord r, Eq l) => GameStateView l cn r ph -> l -> r -> Maybe Int
viewHowManyAt g l r = flip howMany' r <$> viewLocation g l


-- viewCounterBounds

-- lenses 
useLoc :: Eq l => Player -> l -> Getter (GameState l cn r ph pl i) (Maybe (LocationShape r))
useLoc p l = to $ \gs -> let
            VisibilityMap vis = gs ^. #visibility
            in
            runVis  (vis p (VisLocation l)) (gs ^. #objects . #locations . ftAt l)

useCounterVal ::  Eq cn => Player -> cn -> Getter (GameState l cn r ph pl i) (Maybe Int)
useCounterVal p c = to $ \gs -> let
            VisibilityMap vis = gs ^. #visibility
            in
            runVis (vis p (VisCounter c)) (gs ^. #objects . #counters . ftAt c . #val)

useCounterBounds ::  Eq cn => Player -> cn -> Getter (GameState l cn r ph pl i) (Maybe (Int, Int))
useCounterBounds p c = to $ \gs -> let
            VisibilityMap vis = gs ^. #visibility
            in
            runVis (vis p (VisCounter c)) (gs ^. #objects . #counters . ftAt c . #bounds)

useCurrentPhase :: Player ->  Getter (GameState l cn r ph pl i) (Maybe ph)
useCurrentPhase p = to $ \gs -> let
            VisibilityMap vis = gs ^. #visibility
            in
            runVis (vis p VisCurrentPhase) (gs ^. #currentPhase)

useTurnOwner :: Player -> Getter (GameState l cn r ph pl i) (Maybe Player)
useTurnOwner p = to $ \gs -> let
            VisibilityMap vis = gs ^. #visibility
            in
            runVis (vis p VisCurrentPhase) (gs ^. #currentTurn . #owner)


--- functions


queryLocations :: (Eq l, Finitary l, Ord r, Ord l) => (l -> Bool) -> (r -> Bool) -> GameEff l cn r ph pl i (Map l (Map r Int))
queryLocations lfilt rfilt = fmap (M.filter (not . null) . fmap (M.filterWithKey (\r _ -> rfilt r) . inventory))       . sequence
    $ M.fromSet lookLocation (S.filter lfilt inhabitantsSet)

queryLocationsHas :: (Finitary l, Ord r, Ord l) => (l -> Bool) -> (r -> Bool) -> GameEff l cn r ph pl i (Map l [r])
queryLocationsHas lfilt rfilt = fmap M.keys <$> queryLocations lfilt rfilt

queryResources :: (Eq l, Finitary l, Ord r, Ord l) => (l -> Bool) -> (r -> Bool) -> GameEff l cn r ph pl i (Map r (Map l Int))
queryResources lfilt rfilt = invertNestedMaps <$> queryLocations lfilt rfilt

queryResourcesAt :: (Finitary l, Ord r, Ord l) => (l -> Bool) -> (r -> Bool) -> GameEff l cn r ph pl i (Map r [l])
queryResourcesAt lfilt rfilt = fmap M.keys <$> queryResources lfilt rfilt

listResAtF :: (Ord r, Eq l, Finitary l, Ord l) => l -> (r -> Bool) -> GameEff l cn r ph pl i [r]
listResAtF l filt = M.keys <$> queryResourcesAt (== l) filt

listResAt :: (Ord r, Eq l, Finitary l, Ord l) => l -> GameEff l cn r ph pl i [r]
listResAt l = listResAtF l (const True)

findResourceWithin' ::  (Ord r, Eq l, Finitary l, Ord l) =>  r -> [l] -> GameEff l cn r ph pl i [l]
findResourceWithin' r locationNames = M.keys <$> queryLocations (`elem` locationNames) (== r)

notWithin :: (Ord r, Eq l, Finitary l, Ord l) => r -> [l] -> GameEff l cn r ph pl i Bool
notWithin r locNames = null <$> findResourceWithin' r locNames

howManyAt :: (Ord r, Eq l) => l -> r -> GameEff l cn r ph pl i Int
howManyAt l r = flip howMany' r <$> lookLocation l

howManyAt' :: Ord r => Locations l r -> l -> r -> Int
howManyAt' locs l = howMany' (locs !!! l)


howManyAtF :: (Ord r, Eq l) => l -> r -> (Int -> Bool) -> GameEff l cn r ph pl i Bool
howManyAtF l r pred = pred <$> howManyAt l r

howManyWithin ::  (Ord r, Eq l) => [l] -> r -> GameEff l cn r ph pl i Int
howManyWithin ls r = sum ((`howManyAt` r) <$> ls)

howManyWithinF ::  (Ord r, Eq l) => l -> [r] -> (Int -> Bool) -> GameEff l cn r ph pl i Bool
howManyWithinF l rs pred = pred <$> sum (howManyAt l <$> rs)


peek :: Eq l => l -> GameEff l cn r ph pl i (Maybe r)
peek l =  peek' <$> lookLocation l

has ::  (Ord r, Eq l) =>  l -> r -> GameEff l cn r ph pl i Bool
has l r = (> 0) <$> howManyAt l r

has'' :: Ord r =>  l -> r ->Locations l r -> Bool
has'' l r locs = howManyAt' locs l r > 0

hasMaybe :: (Ord a, Eq l) => l -> a -> GameEff l cn a ph pl i (Maybe a)
hasMaybe l r = do
    i <- howManyAt l r
    return $ if i > 0
    then Just r
    else Nothing

doesNotHave ::  (Ord r, Eq l) =>  l -> r -> GameEff l cn r ph pl i Bool
doesNotHave l r = not <$> has l r

doesNotHave' :: Ord r =>  l -> r -> Locations l r -> Bool
doesNotHave' l r locs = not (has'' l r locs)


mkTransfer :: l -> l -> r -> GameNode l cn r ph pl i
mkTransfer l l' r = action (MkTransfer l l' r)

justTransfer :: Applicative f => l -> l -> r -> f [GameNode l cn r ph pl i]
justTransfer l l' r = pure [mkTransfer l l' r]

mkSwap :: l -> l -> r -> r -> GameNode l cn r ph pl i
mkSwap l l' r r' = action (MkSwap l l' r r')

nodeMaybe :: (a -> GameNode l cn r ph pl i) -> Maybe a -> [GameNode l cn r ph pl i]
nodeMaybe f = maybe [action DoNothing] ((:[]) . f)

anyHas :: (Ord r, Eq l) => l -> [r]  -> GameEff l cn r ph pl i Bool
anyHas l = fmap or . traverse (has l)

transferAll :: (Ord r, Eq l) => l -> l -> r -> GameEff l cn r ph pl i [GameNode l cn r ph pl i]
transferAll source target res = replicate <$> howManyAt source res <*> pure (mkTransfer source target res)

whatsAt :: (Ord r, Eq l) => l -> GameEff l cn r ph pl i (Set r)
whatsAt loc = M.keysSet  . M.filter (>0) . inventory <$> lookLocation loc

-- unsafeSwapAll :: l -> l -> GameEff l cn r ph pl i [GameNode l cn r ph pl i]
unsafeSwapAll :: (Eq l, Ord r, Finitary l, Ord l) => l -> l -> GameEff l cn r ph pl i [GameNode l cn r ph pl i]
unsafeSwapAll l0 l1 = (fmap (mkTransfer l0 l1) <$> listResAt l0)
                      <> (fmap (mkTransfer l1 l0) <$> listResAt l1)


revealTo :: l -> Player -> GameEff l cn r ph pl i [GameNode l cn r ph pl i]
revealTo loc p = pure [action $ MakeVisibleTo p (VisLocation loc)]

unrevealTo :: Applicative f => l -> Player -> f [GameNode l cn r ph pl i]
unrevealTo loc p = pure [action $ MakeInvisibleTo p (VisLocation loc)]

(<+) :: Enum a => a -> Int -> a
a <+ i
  | i > 0 = succ a <+ (i - 1)
  | i < 0 = pred a <+ (i + 1)
  | otherwise = a

----- Options stuff

baseOptions :: Player -> NonEmpty pl -> Options pl i
baseOptions p legals = Options legals M.empty p


