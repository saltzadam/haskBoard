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
module Location where
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Count
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Lens
import Data.Array (Array)
import Data.Array as A
import Data.Generics.Labels
import Control.Monad (guard)
import Data.Foldable (toList)


-- Goal of this module is to enforce 'conversion of pieces', not game rules.
-- Right now OLoc and ULoc are "piles of stuff" and "FIFO decks." Could work
-- on other things. If you need more control over placement then should probably
-- be creating more locations rather than more complicated location types.

--- Games can have multiple resource types!
-- data FResource deriving (Eq, Ord, Show, Generic)
-- data NFResource deriving (Eq, Ord, Show, Generic)
-- for fungible and non-fungible
--
-- Can't subtype them (easily) but shouldn't be an issue?


data OrderedLocation name resource = OLoc  {name :: name,
                                            permitted :: [resource],
                                            stuff :: Seq resource}
                                            deriving (Eq, Ord, Generic, Show)

data UnorderedLocation name resource = ULoc {name :: name,
                                             stuff :: Map resource (Cnt Int)
                                            } deriving (Eq, Ord, Show, Generic)

makeFields ''UnorderedLocation
makeFields ''OrderedLocation

class Location l n r where
    moveFrom :: r -> l n r -> (l n r, Maybe r)
    moveTo :: r -> l n r -> (l n r, Maybe r)
    inventory :: l n r -> Map r (Cnt Int)

instance Ord r => Location UnorderedLocation n r where
    moveFrom r l@(ULoc _ s) = case fmap (> 0) (M.lookup r s) of
                              Nothing -> (l, Nothing)
                              Just False -> (l, Nothing)
                              Just True -> (over (#stuff . at r . mapped) (subtract 1) l, Just r)
    moveTo r l@(ULoc _ s) = if r `M.member` s
                            then (over (#stuff . at r . mapped) (+1) l, Just r)
                            else (l, Nothing)
    inventory (ULoc _ s) = s

instance Ord r => Location OrderedLocation n r where
    moveFrom r l = case view (#stuff . to (Seq.elemIndexL r))  l of
            Nothing -> (l, Nothing)
            Just i -> (over #stuff (Seq.deleteAt i) l, Just r)
    moveTo r l@(OLoc _ p _) = if r `notElem` p then (l, Nothing)
                              else (over #stuff (r <|) l, Just r)
    inventory (OLoc _ _ s) = histogramF s

-- laws!
transfer :: (Location l n r, Location l' n' r) => r -> l n r -> l' n' r -> (l n r, l' n' r, Maybe r)
transfer r loc loc' = case moveFrom r loc of 
                        (newLoc, Just r) -> case moveTo r loc' of 
                                                 (newLoc', Just r) -> (newLoc, newLoc', Just r)
                                                 (_, Nothing) -> (loc, loc', Nothing)
                        (_, Nothing) -> (loc, loc', Nothing)

peek :: OrderedLocation n r -> Maybe r
peek (OLoc _ _ s) = Seq.lookup 0 s

draw :: OrderedLocation n r -> (OrderedLocation n r, Maybe r)
draw l = case peek l of
                        Nothing -> (l, Nothing)
                        Just r -> (over #stuff (Seq.drop 0) l, Just r)

countPieces :: Location l n r => l n r -> Cnt Int
countPieces = sum . inventory

data GameLocations lnames fresources nfresources =
    GameLocations {decks :: Map lnames (OrderedLocation lnames nfresources),
                   piles :: Map lnames (UnorderedLocation lnames fresources),
                   queues :: Map lnames (OrderedLocation lnames fresources),
                   hands :: Map lnames (UnorderedLocation lnames nfresources)}
                   deriving (Eq, Ord, Show, Generic)

makeFields ''GameLocations

emptyLocs :: GameLocations lnames fresources nfresources
emptyLocs = GameLocations M.empty M.empty M.empty M.empty

addPile :: Ord l => l -> Map f (Cnt Int) -> GameLocations l f n -> GameLocations l f n
addPile name stuff = set (#piles . at name) (Just (ULoc name stuff))

addDeck :: Ord l => l -> [n] -> Seq n -> GameLocations l f n -> GameLocations l f n
addDeck name permitted stuff = set (#decks . at name) (Just (OLoc name permitted stuff))

addFullDeck :: Ord l => l -> Seq n -> GameLocations l f n -> GameLocations l f n
addFullDeck name fullStuff = addDeck name (toList fullStuff) fullStuff

addEmptyDeck :: Ord l => l -> Seq n -> GameLocations l f n -> GameLocations l f n
addEmptyDeck name = addDeck name (Seq.empty)

addQueue :: Ord l => l -> [f] -> Seq f -> GameLocations l f n -> GameLocations l f n
addQueue name permitted stuff = set (#queues . at name) (Just (OLoc name permitted stuff))

addHand :: Ord l => l -> Map n (Cnt Int) -> GameLocations l f n -> GameLocations l f n
addHand name stuff = set (#hands . at name) (Just (ULoc name stuff))

addHandUnique :: (Ord n, Ord l) => l -> [n] -> [n] -> GameLocations l f n -> GameLocations l f n
addHandUnique name permitted listOfSingles = let
    theMap = M.fromList ([(s,1) | s <- listOfSingles] ++ [(s,0) | s <- permitted])
    in
        set (#hands . at name) (Just (ULoc name theMap))



