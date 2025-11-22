{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$" #-}
module Helpers where

import Control.Lens (to, view, (^.))
import Control.Monad (replicateM_, void)
import Control.Monad.Free (liftF)
import Data.Finitary
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import FinitaryMap (ftAt, (!!!))
import GHC.Base (Applicative (..))
import Game.GameAction
import Game.GameState
import Game.Location (Counter, LocationShape, Locations, has', howMany', inventory, listAllShapeF, peek')
import Game.Options
import Game.Player
import Game.Rules
import Game.View (GameStateView)
import Game.Visibility (VisData (..), VisibilityMap (..), runVis)
import Util (buildSafeNonempty, compose, concatNE, getNextCyclic, graph, graphM, ifNullElse, inhabitantsSet, invertNestedMaps, kleisliCompose, mapMaybeMap, mapMaybeMapM)

-- particular actions

roll :: cn -> GameRule l cn r ph pl ()
roll l = liftF (Act (RollCounter l) ())

transfer :: l -> l -> r -> GameRule l cn r ph pl ()
transfer l l' r = act (MkTransfer l l' r)

swap :: l -> l -> r -> r -> GameRule l cn r ph pl ()
swap l l' r r' = act (MkSwap l l' r r')

justDoNothing :: GameRule l cn r ph pl ()
justDoNothing = act DoNothing

revealTo :: l -> Player -> GameRule l cn r ph pl ()
revealTo loc p = act (MakeVisibleTo p (VisLocation loc))

unrevealTo :: l -> Player -> GameRule l cn r ph pl ()
unrevealTo loc p = act (MakeInvisibleTo p (VisLocation loc))

advanceTurn :: GameRule l cn r ph pl ()
advanceTurn = act AdvanceTurn

shuffle :: l -> GameRule l cn r ph pl ()
shuffle = act . Shuffle

endGame :: [Player] -> GameRule l cn r ph pl ()
endGame winners = act (EndGame winners)

announceGame :: Text -> GameRule l cn r ph pl ()
announceGame announcement = act (MakeAnnouncement Nothing announcement)

setPlayerNextTurn :: (Player -> Turn ph) -> Player -> GameRule l cn r ph pl ()
setPlayerNextTurn mkTurn nextP = act (SetNextTurn (Just $ mkTurn nextP))

-- bulk operations
unsafeSwapAll :: (Finitary l, Ord r, Ord l) => l -> l -> GameRule l cn r ph pl ()
unsafeSwapAll l0 l1 = do
  atl0 <- listResAt l0
  atl1 <- listResAt l1
  traverse_ (transfer l0 l1) atl0
  traverse_ (transfer l1 l0) atl1

-- control ops
--

setNextTurnCyclic :: (Player -> Turn ph) -> GameRule l cn r ph pl ()
setNextTurnCyclic mkTurn = do
  ps <- lookPlayers
  p <- lookCurrentTurnOwner
  let ps' = NE.fromList . S.toList $ ps
  act (SetNextTurn (Just $ getNextTurn2 mkTurn ps' p))

getNextTurn2 :: (Player -> Turn ph) -> NE.NonEmpty Player -> Player -> Turn ph
getNextTurn2 mkTurn eligiblePlayers currentPlayer = mkTurn (fromJust (getNextCyclic currentPlayer eligiblePlayers))

getNextTurn :: (Player -> Turn ph) -> GameState l cn r ph pl -> Turn ph
getNextTurn mkTurn = getNextTurnIf mkTurn (\_ _ -> True)

getNextTurnIf :: (Player -> Turn ph) -> (Player -> GameState l cn r ph pl -> Bool) -> GameState l cn r ph pl -> Turn ph
getNextTurnIf mkTurn filt gs =
  let Turn p _ = (gs ^. #currentTurn)
      players = NE.fromList ((S.toList . S.filter (`filt` gs)) (gs ^. #players))
   in mkTurn (fromJust (getNextCyclic p players))

getNextTurnFrom :: (Player -> Turn ph) -> (GameState l cn r ph pl -> NonEmpty Player) -> GameState l cn r ph pl -> Turn ph
getNextTurnFrom mkTurn getPlayers gs =
  let Turn p _ = (gs ^. #currentTurn)
      somePlayers = getPlayers gs
   in mkTurn (fromJust (getNextCyclic p somePlayers))

-- queries

queryLocations :: (Eq l, Finitary l, Ord r, Ord l) => (l -> Bool) -> (r -> Bool) -> GameRule l cn r ph pl (Map l (Map r Int))
queryLocations lfilt rfilt =
  fmap (M.filter (not . null) . fmap (M.filterWithKey (\r _ -> rfilt r) . inventory)) . sequence $
    M.fromSet lookLocation (S.filter lfilt inhabitantsSet)

queryLocationsHas :: (Finitary l, Ord r, Ord l) => (l -> Bool) -> (r -> Bool) -> GameRule l cn r ph pl (Map l [r])
queryLocationsHas lfilt rfilt = fmap M.keys <$> queryLocations lfilt rfilt

queryResources :: (Eq l, Finitary l, Ord r, Ord l) => (l -> Bool) -> (r -> Bool) -> GameRule l cn r ph pl (Map r (Map l Int))
queryResources lfilt rfilt = invertNestedMaps <$> queryLocations lfilt rfilt

queryResourcesAt :: (Finitary l, Ord r, Ord l) => (l -> Bool) -> (r -> Bool) -> GameRule l cn r ph pl (Map r [l])
queryResourcesAt lfilt rfilt = fmap M.keys <$> queryResources lfilt rfilt

listResAtF :: (Ord r, Eq l, Finitary l, Ord l) => l -> (r -> Bool) -> GameRule l cn r ph pl [r]
listResAtF l filt = M.keys <$> queryResourcesAt (== l) filt

listResAt :: (Ord r, Eq l, Finitary l, Ord l) => l -> GameRule l cn r ph pl [r]
listResAt l = listResAtF l (const True)

findResourceWithin' :: (Ord r, Eq l, Finitary l, Ord l) => r -> [l] -> GameRule l cn r ph pl [l]
findResourceWithin' r locationNames = M.keys <$> queryLocations (`elem` locationNames) (== r)

notWithin :: (Ord r, Eq l, Finitary l, Ord l) => r -> [l] -> GameRule l cn r ph pl Bool
notWithin r locNames = null <$> findResourceWithin' r locNames

howManyAt :: (Ord r, Eq l) => l -> r -> GameRule l cn r ph pl Int
howManyAt l r = flip howMany' r <$> lookLocation l

howManyAt' :: (Ord r) => Locations l r -> l -> r -> Int
howManyAt' locs l = howMany' (locs !!! l)

howManyAtF :: (Ord r, Eq l) => l -> r -> (Int -> Bool) -> GameRule l cn r ph pl Bool
howManyAtF l r pred = pred <$> howManyAt l r

howManyWithin :: (Ord r, Eq l) => [l] -> r -> GameRule l cn r ph pl Int
howManyWithin ls r = sum <$> traverse (`howManyAt` r) ls

peek :: (Eq l) => l -> GameRule l cn r ph pl (Maybe r)
peek l = peek' <$> lookLocation l

has :: (Ord r, Eq l) => l -> r -> GameRule l cn r ph pl Bool
has l r = (> 0) <$> howManyAt l r

has'' :: (Ord r) => l -> r -> Locations l r -> Bool
has'' l r locs = howManyAt' locs l r > 0

hasMaybe :: (Ord a, Eq l) => l -> a -> GameRule l cn a ph pl (Maybe a)
hasMaybe l r = do
  i <- howManyAt l r
  return $
    if i > 0
      then Just r
      else Nothing

doesNotHave :: (Ord r, Eq l) => l -> r -> GameRule l cn r ph pl Bool
doesNotHave l r = not <$> has l r

doesNotHave' :: (Ord r) => l -> r -> Locations l r -> Bool
doesNotHave' l r locs = not (has'' l r locs)

anyHas :: (Ord r, Eq l) => l -> [r] -> GameRule l cn r ph pl Bool
anyHas l = fmap or . traverse (has l)

hasAny :: (Ord r, Eq l) => l -> [r] -> GameRule l cn r ph pl Bool
hasAny = anyHas

transferAll :: (Ord r, Eq l) => l -> l -> r -> GameRule l cn r ph pl ()
transferAll source target res = howManyAt source res >>= (`replicateM_` transfer source target res)

whatsAt :: (Ord r, Eq l) => l -> GameRule l cn r ph pl (Set r)
whatsAt loc = M.keysSet . M.filter (> 0) . inventory <$> lookLocation loc

(<+) :: (Enum a) => a -> Int -> a
a <+ i
  | i > 0 = succ a <+ (i - 1)
  | i < 0 = pred a <+ (i + 1)
  | otherwise = a

----- Options stuff

baseOptions :: Player -> NonEmpty pl -> Options pl
baseOptions p legals = Options legals p

counterAtMax :: (Eq cn) => cn -> GameRule l cn r ph pl Bool
counterAtMax cname = liftA2 (==) (lookCounterVal cname) (snd <$> lookCounterBounds cname)

-- view for GameView

viewLocation :: (Eq l) => GameStateView l cn r ph -> l -> Maybe (LocationShape r)
viewLocation gsv l = gsv ^. #objectsView . #locationsView . ftAt l

viewCounterVal :: (Eq cn) => GameStateView l cn r ph -> cn -> Maybe Int
viewCounterVal gsv cn = view #val <$> gsv ^. #objectsView . #countersView . ftAt cn

viewCurrentPlayer :: GameStateView l cn r ph -> Player
viewCurrentPlayer gsv = gsv ^. #currPlayer

viewHowManyAt :: (Ord r, Eq l) => GameStateView l cn r ph -> l -> r -> Maybe Int
viewHowManyAt g l r = flip howMany' r <$> viewLocation g l

-- for actions

activePlayer :: (Player -> GameRule l cn r ph pl a) -> GameRule l cn r ph pl a
activePlayer action = lookCurrentTurnOwner >>= action

lookOtherPlayers :: Player -> GameRule l cn r ph pl (Set Player)
lookOtherPlayers p = S.filter (/= p) <$> lookPlayers
