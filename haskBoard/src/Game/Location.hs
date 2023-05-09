{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Game.Location
    (LocationShape(..),
     Locations,
     transfer,
     inventory,
     howMany',
     has',
     findResourceWithin,
     findResource,
     listAll,
     listAllF,
     listAllShape,
     peek,
     Counter(..),
     Counters,
     makeCounter,
     d6,
     setCounter,
     increment,
     decrement,
     GameObjects(..),
     howMany,
     look,
     dummyCounter,
     howManyF
    )
 where
import Control.Lens (makeFields, set)
import Count
import Data.Generics.Labels ()
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Sequence (Seq ((:<|), Empty), (<|))
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import FinitaryMap (FTMap (..), (!!!), FakeFinitary, inhabitants)
import qualified FinitaryMap as FT
import Game.Visibility (VisibilityType (..))


---- Definitions and instances

data LocationShape r = Deck (Seq r) -- Ordered items. Transfers from the topmost, transfers to the top.
                     | Pile (Map r (Cnt Int)) -- Unordered items
                     | Slot (Maybe r) -- Single slot
                     | Dummy -- No space
 deriving (Eq, Ord, Show, Generic)

type Locations names r = FTMap names (LocationShape r)

-- Transfer should not happen unless sender and recipient allow it.
-- This enforces the invariant that resources cannot 'disappear.' Either
-- they will stay with the sender or they get transferred.
--
data TransferStatus = Success | Failure deriving (Eq, Ord, Show, Generic)

-- maybeToSuccess :: Maybe r -> TransferStatus
-- maybeToSuccess (Just _) = Success
-- maybeToSuccess Nothing = Failure

-- TODO: should be abstractable


moveFromL :: Ord r => r -> LocationShape r -> (LocationShape r, TransferStatus)
moveFromL r (Deck s) = case Seq.elemIndexL r s of -- search from left (i.e. "top")
                        Nothing -> (Deck s, Failure)
                        Just i -> (Deck (Seq.deleteAt i s), Success)
moveFromL r (Pile pileMap) = case M.lookup r pileMap of
                              Nothing -> (Pile pileMap, Failure)
                              Just i -> if i > 0
                                        then (Pile $ M.adjust (subtract 1) r pileMap, Success)
                                        else (Pile pileMap, Failure)
moveFromL _ (Slot Nothing) = (Slot Nothing, Failure)
moveFromL r (Slot (Just r')) = if r' == r
                              then (Slot Nothing, Success)
                              else (Slot (Just r'), Failure)
moveFromL _ Dummy = (Dummy, Failure)


moveToL :: Ord r => r -> LocationShape r -> (LocationShape r, TransferStatus)
moveToL r (Deck s) = (Deck (r <| s), Success) -- add to left (i.e. "top")
moveToL r (Pile pileMap) = (Pile (M.alter addOneWithDefault r pileMap), Success) where
    addOneWithDefault Nothing = Just 1
    addOneWithDefault (Just i) = Just (i+1)
                           -- if r `M.member` pileMap
                           -- then (Pile (M.adjust (+1) r pileMap), Success)
                           -- else (Pile pileMap, Failure)
moveToL r (Slot Nothing) = (Slot (Just r), Success)
moveToL _ (Slot (Just r')) = (Slot (Just r'), Failure)
moveToL _ Dummy = (Dummy, Failure)


-- laws!
-- The difficulty with source == target comes from thinking imperatively: "Move from here and to here"
-- OTOH that is the plain meaning of "transfer". Arguable what
-- "transfer from a deck to itself" means. Probably has to mean "move
-- topmost r to the top of the deck" to preserve laws.
--
-- Actually, can't even meaningfully test shapes for equality! Player A
-- and Player B might start with the same set of resources! Check has to
-- happen below.
transfer' :: Ord r => r -> LocationShape r -> LocationShape r -> (LocationShape r -> LocationShape r, LocationShape r -> LocationShape r, TransferStatus)
transfer' r source target =
    let
        (_, sourceStatus) = moveFromL r source
        (_, targetStatus) = moveToL r target
    in
        if sourceStatus == Failure || targetStatus == Failure
                then (id,id, Failure)
                else (fst . moveFromL r , fst . moveToL r, Success)


-- if name0 == name1 then loc0 == loc1
-- I think this works if we just swap the order of the two `update`s.
-- If we've already checked for success, then the order of those doesn't
-- matter in most situations. It's like -1 to one pile and +1 to
-- another. But if source == target, then we have to remove the thing
-- before adding it.

transferSafelyByName :: (Ord name, Ord r) => r -> name -> name -> Locations name r -> (Locations name r, TransferStatus)
transferSafelyByName r name0 name1 locs = let
    loc0 = locs !!! name0
    loc1 = locs !!! name1
    (sourceF,targetF, mayber) = transfer' r loc0 loc1
    in
        case mayber of
          Failure-> (locs, Failure)
          Success -> (FT.applyAt name1 targetF . FT.applyAt name0 sourceF $ locs, Success)

transfer :: (Ord name, Ord r) => r -> name -> name -> Locations name r -> Locations name r
transfer r n0 n1 l = fst (transferSafelyByName r n0 n1 l)

----- Querying


-- why not inventory = histogramF :(
inventory :: Ord r => LocationShape r -> (Map r) (Cnt Int)
inventory (Pile s) = s
inventory (Deck s) = histogramF s
inventory (Slot Nothing) = M.empty
inventory (Slot (Just r)) = M.singleton r 1
inventory Dummy = M.empty

howManyF :: Ord r => LocationShape r -> (r -> Bool) -> Cnt Int
howManyF loc filt = sum . M.filterWithKey (\k _ -> filt k) . inventory $ loc

howMany' :: Ord r => LocationShape r -> r -> Cnt Int
howMany' loc res = fromMaybe 0 . M.lookup res . inventory $ loc

howMany :: Ord r => Locations l r -> l -> r -> Cnt Int
howMany locs lname = howMany' (locs !!! lname)

has' :: Ord r => LocationShape r -> r -> Bool
has' loc r = howMany' loc r > 0

findResourceWithin :: Ord r => r -> [n] -> Locations n r -> [n]
findResourceWithin res names locs = filter (\n -> (locs !!! n) `has'` res) names

findResource :: (FakeFinitary n, Eq r, Ord r) => r -> Locations n r -> [n]
findResource res = findResourceWithin res inhabitants

listAll :: Ord r => n -> Locations n r -> [r]
listAll n locs = listAllF n locs (const True)

listAllShapeF :: Ord a => LocationShape a -> (a -> Bool) -> [a]
listAllShapeF locs filt = filter filt . M.keys . M.filter (>0) . inventory $ locs

listAllShape :: Ord a => LocationShape a -> [a]
listAllShape locs = listAllShapeF locs (const True)

listAllF :: Ord r => n -> Locations n r -> (r -> Bool) -> [r]
listAllF n locs = listAllShapeF (locs !!! n)
-- listAllF n locs filt = filter filt . M.keys . M.filter (>0) . inventory $ locs !!! n

peek :: LocationShape r -> Maybe r
peek (Pile s) = listToMaybe (M.keys . M.filter (>0) $ s)
peek (Deck (x :<| _)) = Just x
peek (Deck Empty) = Nothing
peek (Slot s) = s
peek Dummy = Nothing

---- Counters

data Counter = Counter {val :: Cnt Int,
                        bounds :: (Cnt Int, Cnt Int)} deriving (Eq, Show, Generic)

makeFields ''Counter

type Counters name = FTMap name Counter

makeCounter :: (Cnt Int, Cnt Int) -> Counter
makeCounter (a, b) = Counter a (a, b)

dummyCounter :: Counter
dummyCounter = Counter 0 (0,0)

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

--- visibility
-- This is kind of silly but adds flexibility for "top card of the deck is always visible"
look :: LocationShape r -> VisibilityType -> LocationShape r
look l Visible = l
look _ Invisible = Dummy


data GameObjects n cn r = GameObjects {
    locations :: Locations n r,
    counters :: Counters cn} deriving (Generic, Show)

makeFields ''GameObjects


