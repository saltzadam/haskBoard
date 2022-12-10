{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module Location where

import Control.Lens (makeFields)
import Count
-- import Data.Array (Array)
import Data.Generics.Labels ()
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.Sequence (Seq ((:<|), Empty), (<|))
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)

-- Goal of this module is to enforce 'conversion of pieces', not game rules.
-- Right now OLoc and ULoc are "piles of stuff" and "FIFO decks." Could work
-- on other things. If you need more control over placement then should probably
-- be creating more locations rather than more complicated location types.

data LocationShape r = Deck (Seq r) | Pile (Map r (Cnt Int)) | Slot (Maybe r) | Dummy -- | Counter Int (Maybe Int, Maybe Int)
 deriving (Eq, Ord, Show, Generic)

type Locations names r = Map names (LocationShape r)

moveFromL :: Ord r => r -> LocationShape r -> (LocationShape r, Maybe r)
moveFromL r (Deck s) = case Seq.elemIndexL r s of
                        Nothing -> (Deck s, Nothing)
                        Just i -> (Deck (Seq.deleteAt i s), Just r)
moveFromL r (Pile pileMap) = case M.lookup r pileMap of
                              Nothing -> (Pile pileMap, Nothing)
                              Just i -> if i > 0
                                        then (Pile (M.adjust (subtract 1) r pileMap), Just r)
                                        else (Pile pileMap, Nothing)
moveFromL _ (Slot Nothing) = (Slot Nothing, Nothing)
moveFromL r (Slot (Just r')) = if r' == r
                              then (Slot Nothing, Just r)
                              else (Slot (Just r'), Nothing)
moveFromL _ Dummy = (Dummy, Nothing)


moveToL :: Ord r => r -> LocationShape r -> (LocationShape r, Maybe r)
moveToL r (Deck s) = (Deck (r <| s), Just r) 
moveToL r (Pile pileMap) = if r `M.member` pileMap
                           then (Pile (M.adjust (+1) r pileMap), Just r)
                           else (Pile pileMap, Nothing)
moveToL r (Slot Nothing) = (Slot (Just r), Just r)
moveToL _ (Slot (Just r')) = (Slot (Just r'), Nothing)
moveToL _ Dummy = (Dummy, Nothing)

inventory :: Ord r => LocationShape r -> Map r (Cnt Int)
inventory (Pile s) = s
inventory (Deck s) = histogramF s
inventory (Slot Nothing) = M.empty
inventory (Slot (Just r)) = M.singleton r 1
inventory Dummy = M.empty

-- laws!
transfer' :: Ord r => r -> LocationShape r -> LocationShape r -> (LocationShape r, LocationShape r, Maybe r)
transfer' r loc loc' = case moveFromL r loc of
  (newLoc, Just r) -> case moveToL r loc' of
    (newLoc', Just r) -> (newLoc, newLoc', Just r)
    (_, Nothing) -> (loc, loc', Nothing)
  (_, Nothing) -> (loc, loc', Nothing)

transfer :: (Ord name, Ord r) => r -> name -> name -> Locations name r -> (Locations name r, Maybe r)
transfer r name0 name1 locs = let
    loc0 = locs M.! name0
    loc1 = locs M.! name1
    (loc0',loc1',mayber) = transfer' r loc0 loc1
    in
        case mayber of
          Nothing -> (locs, Nothing)
          Just i -> (M.insert name0 loc0' . M.insert name1 loc1' $ locs, Just i)

peek :: LocationShape r -> Maybe r
peek (Pile s) = listToMaybe (M.keys . M.filter (>0) $ s)
peek (Deck (x :<| _)) = Just x
peek (Deck Empty) = Nothing
peek (Slot s) = s
peek Dummy = Nothing

countPieces :: Ord r => LocationShape r -> Cnt Int
countPieces = sum . inventory

data Counter = Counter {val :: Maybe (Cnt Int),
                        bounds :: (Cnt Int, Cnt Int)} deriving (Eq, Show)

makeCounter :: (Cnt Int, Cnt Int) -> Counter
makeCounter (a, b) = Counter (Just a) (a, b)

mapCounter :: (Cnt Int -> Cnt Int) -> Counter -> (Counter, Maybe (Cnt Int))
mapCounter _ c@(Counter Nothing (_,_)) = (c, Nothing)
mapCounter f c@(Counter (Just a) (bl, bu)) = if f a >= bl && f a <= bl 
                                    then (Counter (Just (f a)) (bl, bu), Just (f a))
                                    else (c, Nothing)

increment :: Counter -> (Counter, Maybe (Cnt Int))
increment = mapCounter (+1)

decrement :: Counter -> (Counter, Maybe (Cnt Int))
decrement = mapCounter (subtract 1)

data GameObjects n r = GameObjects {
    locations :: Locations n r,
    counters :: Map n Counter} deriving (Eq, Show, Generic)

makeFields ''GameObjects


-- addPile :: Ord u => u -> Map r (Cnt Int) -> GameObjects o u s r -> GameObjects o u s r
-- addPile name stuff = set (#piles . at name) (Just (PileL stuff))

-- addDeck :: Ord o => o -> Seq r -> GameObjects o u s r -> GameObjects o u s r
-- addDeck name stuff = set (#decks . at name) (Just (DeckL stuff))

-- -- addFullDeck :: Ord o => o -> Seq r -> GameObjects o u r -> GameObjects o u r
-- -- addFullDeck name fullStuff = addDeck name (toList fullStuff) fullStuff

-- addEmptyDeck :: Ord o => o -> GameObjects o u s r -> GameObjects o u s r
-- addEmptyDeck name = addDeck name Seq.empty

-- addPileCnt :: (Ord r, Ord u) => u -> [r] -> Cnt Int -> GameObjects o u s r -> GameObjects o u s r
-- addPileCnt name listOfSingles cnt =
--   let theMap = M.fromList ([(s, cnt) | s <- listOfSingles])
--    in set (#piles . at name) (Just (PileL theMap))

-- addCounter :: Ord u => u -> Cnt Int -> GameObjects o u s r -> GameObjects o u s r
-- addCounter name i = set (#counters . at name) (Just i)
