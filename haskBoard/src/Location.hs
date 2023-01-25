{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Location where

import Control.Lens (makeFields, set, view)
import Count
-- import Data.Array (Array)
import Data.Generics.Labels ()
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe, isJust, mapMaybe)
import Data.Sequence (Seq ((:<|), Empty), (<|))
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import System.Random.Stateful (uniformR)
import Defaultable.Map (Defaultable(..))
import qualified Defaultable.Map as D
import Control.Monad.State
import Control.Monad.Random (RandomGen)
import FinitaryMap (FTMap (..), (!!!))
import qualified FinitaryMap as FT
import Data.Finitary

data LocationShape r = Deck (Seq r) | Pile (D.Defaultable (Map r) (Cnt Int)) | Slot (Maybe r) | Dummy
 deriving (Eq, Ord, Show, Generic)

type Locations names r = FTMap names (LocationShape r)

defaultToSearchableList :: (Ord r, Finitary r) => D.Defaultable (Map r) (Cnt Int) -> [r]
defaultToSearchableList dmap = go (finDefaultToList dmap) [] where
    -- for each step:   look only at keys with positive values
    --                  add those keys to the result
    --                  subtract one from all values
    --                  repeat until there are no keys left
    go :: [(r,Cnt Int)] -> [r] -> [r]
    go pairs result = if null pairs then result
                        else go (fmap (subtract 1) <$> pairs) (result ++ fmap fst pairs)
    finDefaultToList :: (Ord r, Finitary r) => D.Defaultable (Map r) a -> [(r,a)]
    finDefaultToList dmap' =  mapMaybe (\r -> sequence (r, D.lookup r dmap')) inhabitants


dadjust :: Ord k => (a -> a) -> k -> Defaultable (Map k) a -> Defaultable (Map k) a
dadjust f k m = D.singleton (k,f) <*> m

moveFromL :: Ord r => r -> LocationShape r -> (LocationShape r, Maybe r)
moveFromL r (Deck s) = case Seq.elemIndexL r s of
                        Nothing -> (Deck s, Nothing)
                        Just i -> (Deck (Seq.deleteAt i s), Just r)
moveFromL r (Pile pileMap) = case D.lookup r pileMap of
                              Nothing -> (Pile pileMap, Nothing)
                              Just i -> if i > 0
                                        then (Pile $ dadjust (subtract 1) r pileMap, Just r)
                                        else (Pile pileMap, Nothing)
moveFromL _ (Slot Nothing) = (Slot Nothing, Nothing)
moveFromL r (Slot (Just r')) = if r' == r
                              then (Slot Nothing, Just r)
                              else (Slot (Just r'), Nothing)
moveFromL _ Dummy = (Dummy, Nothing)


moveToL :: Ord r => r -> LocationShape r -> (LocationShape r, Maybe r)
moveToL r (Deck s) = (Deck (r <| s), Just r)
moveToL r (Pile pileMap) = if r `M.member` D.toMap pileMap
                           then (Pile (dadjust (+1) r pileMap), Just r)
                           else (Pile pileMap, Nothing)
moveToL r (Slot Nothing) = (Slot (Just r), Just r)
moveToL _ (Slot (Just r')) = (Slot (Just r'), Nothing)
moveToL _ Dummy = (Dummy, Nothing)

inventory :: Ord r => LocationShape r -> Defaultable (Map r) (Cnt Int)
inventory (Pile s) = s
inventory (Deck s) = histogramF s
inventory (Slot Nothing) = Defaultable M.empty (Just 0)
inventory (Slot (Just r)) = D.singleton (r,1)
inventory Dummy = Defaultable M.empty (Just 0)

has' :: Ord r => LocationShape r -> r -> Bool
has' loc r = case fmap (>0) (D.lookup r (inventory loc)) of
              Just True -> True
              _ -> False



findResourceWithin :: Ord r => r -> [n] -> Locations n r -> [n]
findResourceWithin res names locs = filter (\n -> (locs !!! n) `has'` res) names

findResource :: (Finitary n, Eq r, Ord r) => r -> Locations n r -> [n]
findResource res = findResourceWithin res inhabitants

--
-- laws!
transfer' :: Ord r => r -> LocationShape r -> LocationShape r -> (LocationShape r, LocationShape r, Maybe r)
transfer' r loc loc' = case moveFromL r loc of
  (newLoc, Just r) -> case moveToL r loc' of
    (newLoc', Just r) -> (newLoc, newLoc', Just r)
    (_, Nothing) -> (loc, loc', Nothing)
  (_, Nothing) -> (loc, loc', Nothing)



transferSafe :: (Ord name, Ord r) => r -> name -> name -> Locations name r -> (Locations name r, Maybe r)
transferSafe r name0 name1 locs = let
    loc0 = locs !!! name0
    loc1 = locs !!! name1
    (loc0',loc1',mayber) = transfer' r loc0 loc1
    in
        case mayber of
          Nothing -> (locs, Nothing)
          Just i -> (FT.update (name0, loc0') . FT.update (name1, loc1') $ locs, Just i)

transfer :: (Ord name, Ord r) => r -> name -> name -> Locations name r -> Locations name r
transfer r n0 n1 l = fst (transferSafe r n0 n1 l)

peek :: LocationShape r -> Maybe r
peek (Pile s) = listToMaybe (M.keys . M.filter (>0) $ D.toMap s)
peek (Deck (x :<| _)) = Just x
peek (Deck Empty) = Nothing
peek (Slot s) = s
peek Dummy = Nothing

countPieces :: Ord r => LocationShape r -> Cnt Int
countPieces = sum . inventory

data Counter = Counter {val :: Cnt Int,
                        bounds :: (Cnt Int, Cnt Int)} deriving (Eq, Show, Generic)

makeFields ''Counter

type Counters name = FTMap name Counter

makeCounter :: (Cnt Int, Cnt Int) -> Counter
makeCounter (a, b) = Counter a (a, b)

d6 :: Counter
d6 = makeCounter (Cnt 1, Cnt 6)

mapCounter :: (Cnt Int -> Cnt Int) -> Counter -> (Counter, Maybe (Cnt Int))
mapCounter f c@(Counter a (bl, bu)) = if f a >= bl && f a <= bl
                                    then (Counter (f a) (bl, bu), Just (f a))
                                    else (c, Nothing)

setCounter :: Counter -> Cnt Int -> Counter
setCounter c a = set #val a c

increment' :: Counter -> (Counter, Maybe (Cnt Int))
increment' = mapCounter (+1)

increment :: Counter -> Counter
increment = fst . increment'

decrement' :: Counter -> (Counter, Maybe (Cnt Int))
decrement' = mapCounter (subtract 1)

decrement :: Counter -> Counter
decrement = fst . decrement'

-- TODO: this is still not the right type signature. Want something like
-- StateT g m Counter 
-- with more/different constraints
rollCounter :: (Monad m, RandomGen g, MonadState Counter m) => Counter -> StateT g m Counter
rollCounter c = do
    let (bl,bu) = view #bounds c
    newVal <- state (uniformR (bl,bu))
    let c' = set #val newVal c
    return c'


data GameObjects n cn r = GameObjects {
    locations :: Locations n r,
    counters :: Counters cn} deriving (Generic, Show)

makeFields ''GameObjects



