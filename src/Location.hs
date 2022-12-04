{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
    {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DerivingVia #-}
{-# Language TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Location where
import Count
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Lens
-- import Data.Array (Array)
import Data.Generics.Labels()
import Data.Maybe (isNothing)


-- Goal of this module is to enforce 'conversion of pieces', not game rules.
-- Right now OLoc and ULoc are "piles of stuff" and "FIFO decks." Could work
-- on other things. If you need more control over placement then should probably
-- be creating more locations rather than more complicated location types.

--- Games can have multiple resource types! --< NO, switch to one resource type.
-- data FResource deriving (Eq, Ord, Show, Generic)
-- data NFResource deriving (Eq, Ord, Show, Generic)
-- for fungible and non-fungible
--


-- data OrderedLocation resource = OLoc {stuff :: Seq resource} deriving (Eq, Ord, Generic, Show)

-- data UnorderedLocation resource = ULoc {stuff :: Map resource (Cnt Int)} deriving (Eq, Ord, Show, Generic)

data OrderedLocation deriving (Generic, Show, Eq, Ord)
data UnorderedLocation deriving (Generic, Show, Eq, Ord)
data SingletonLocation deriving (Generic, Show, Eq, Ord)

data Location t resource where
    PileL :: Map resource (Cnt Int) -> Location UnorderedLocation resource
    DeckL :: Seq resource -> Location OrderedLocation resource -- add permitted
    SingleL :: Maybe resource -> Location SingletonLocation resource

deriving instance (Eq r, Eq t) => Eq (Location t r)
deriving instance (Ord r, Ord t) => Ord (Location t r)
deriving instance (Show r, Show t) => Show (Location t r)
-- makeFields ''UnorderedLocation
-- makeFields ''OrderedLocation
-- makeFields ''Location

lensPile :: Lens' (Location UnorderedLocation r) (Map r (Cnt Int))
lensPile f (PileL pile) = fmap PileL (f pile)

lensDeck :: Lens' (Location OrderedLocation r) (Seq r)
lensDeck f (DeckL deck) = fmap DeckL (f deck)

moveFrom :: Ord r => r -> Location t r -> (Location t r, Maybe r)
moveFrom r l@(PileL pile) = case fmap (> 0) (M.lookup r pile) of
                          Nothing -> (l, Nothing)
                          Just False -> (l, Nothing)
                          Just True -> (over (lensPile . at r . mapped) (subtract 1) l, Just r)
moveFrom r l@(DeckL _) = case view (lensDeck . to (Seq.elemIndexL r))  l of
        Nothing -> (l, Nothing)
        Just i -> (over lensDeck (Seq.deleteAt i) l, Just r)
moveFrom r l@(SingleL s) = if s == Just r
                             then (SingleL Nothing, Just r)
                             else (l, Nothing)

moveTo :: Ord r => r -> Location t r -> (Location t r, Maybe r)
moveTo  r l@(PileL s) = if r `M.member` s
                           then (over (lensPile . at r . mapped) (+1) l, Just r)
                           else (l, Nothing)
moveTo  r l@(DeckL _) = (over lensDeck (r <|) l, Just r)
moveTo  r l@(SingleL s) = if isNothing s then (SingleL (Just r), Just r) else (l, Nothing)

inventory :: Ord r => Location t r -> Map r (Cnt Int)
inventory (PileL s) = s
inventory (DeckL s) = histogramF s
inventory (SingleL Nothing) = M.empty
inventory (SingleL (Just r)) = M.singleton r 1

-- laws!
transfer :: Ord r => r -> Location t r -> Location t' r -> (Location t r, Location t' r, Maybe r)
transfer r loc loc' = case moveFrom r loc of
                        (newLoc, Just r) -> case moveTo r loc' of
                                                 (newLoc', Just r) -> (newLoc, newLoc', Just r)
                                                 (_, Nothing) -> (loc, loc', Nothing)
                        (_, Nothing) -> (loc, loc', Nothing)

lookTop :: Location OrderedLocation r -> Maybe r
lookTop (DeckL s) = Seq.lookup 0 s

peek :: Location SingletonLocation r -> Maybe r
peek (SingleL s) = s

draw :: Location OrderedLocation r -> (Location OrderedLocation r, Maybe r)
draw l = case lookTop l of
                        Nothing -> (l, Nothing)
                        Just r -> (over lensDeck (Seq.drop 0) l, Just r)

countPieces :: Ord r => Location t r -> Cnt Int
countPieces = sum . inventory

data GameObjects onames unames resources =
    GameObjects   {piles :: Map unames (Location UnorderedLocation resources),
                   decks :: Map onames (Location OrderedLocation resources),
                   counters :: Map unames (Cnt Int)}
                   deriving (Eq, Ord, Show, Generic)


makeFields ''GameObjects

emptyLocs :: GameObjects o u r
emptyLocs = GameObjects M.empty M.empty M.empty

addPile :: Ord u => u -> Map r (Cnt Int) -> GameObjects o u r -> GameObjects o u r
addPile name stuff = set (#piles . at name) (Just (PileL stuff))

addDeck :: Ord o => o -> Seq r -> GameObjects o u r -> GameObjects o u r
addDeck name stuff = set (#decks . at name) (Just (DeckL stuff))

-- addFullDeck :: Ord o => o -> Seq r -> GameObjects o u r -> GameObjects o u r
-- addFullDeck name fullStuff = addDeck name (toList fullStuff) fullStuff

addEmptyDeck :: Ord o => o -> GameObjects o u r -> GameObjects o u r
addEmptyDeck name = addDeck name Seq.empty

addPileCnt :: (Ord r, Ord u) => u -> [r] -> Cnt Int -> GameObjects o u r -> GameObjects o u r
addPileCnt name listOfSingles cnt = let
    theMap = M.fromList ([(s,cnt) | s <- listOfSingles])
    in
        set (#piles. at name) (Just (PileL theMap))

addCounter :: Ord u => u -> Cnt Int -> GameObjects o u r -> GameObjects o u r
addCounter name i = set (#counters . at name) (Just i)



