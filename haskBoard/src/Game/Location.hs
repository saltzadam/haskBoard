{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Location
  ( LocationShape (..),
    Locations,
    TransferStatus (..),
    moveFromL,
    moveToL,
    transfer',
    -- transferSafelyByName,
    -- -- howManyF,
    -- -- mapCounter,
    -- -- increment',
    -- -- decrement',
    transfer,
    swap,
    inventory,
    howMany',
    has',
    findResourceWithin,
    histogram,
    listAll,
    listAllF,
    listAllShape,
    listAllShapeF,
    peek',
    Counter (..),
    Counters,
    makeCounter,
    d6,
    setCounter,
    increment,
    decrement,
    GameObjects (..),
    howMany, -- TODO: needed?
    dummyCounter,
    howManyF,
    transferCounter',
    transferCounter,
    encodeLocations,
    toGymShape,
    encodeLocationShape,
    encodeCounter,
    fromGymShape,
    decodeLocationShape,
    decodeCounter,
    GymSpace (..),
    locationShapeSpace,
    counterSpace,
    gameObjectsSpace,
    infiniteUpperBound,
    encodeLocationObs,
    encodeCounterObs,
  )
where

import Control.Lens (makeFields, set)
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, decodeStrict, object, (.=))
import Data.Aeson.Types (Value)
import Data.Finitary (Finitary (..), inhabitants)
import Data.Generics.Labels ()
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
import Data.Sequence (Seq (Empty, (:<|)), (<|))
import qualified Data.Sequence as Seq
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as T
import FinitaryMap (FTMap (..), (!!!))
import qualified FinitaryMap as FT
import GHC.Generics (Generic)

---- Definitions and instances
-- TODO: consider defaultable map for Pile and histogram

data LocationShape r
  = Deck (Seq r) -- Ordered items. Transfers from the topmost, transfers to the top.
  | Pile (Map r Int) -- Unordered items
  | Slot (Maybe r) -- Single slot
  | Infinite r -- infinite pile
  | Dummy -- No space
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

type Locations names r = FTMap names (LocationShape r)

data GymFundShape
  = Sequence (Seq Int)
  | MultiDiscrete (Map Int Int)
  | DiscreteM (Maybe Int)
  | DiscreteInf Int
  | Discrete (Int, Int) Int
  | DummyShape
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

toGymShape :: LocationShape Int -> GymFundShape
toGymShape (Deck s) = Sequence s
toGymShape (Pile p) = MultiDiscrete p
toGymShape (Slot s) = DiscreteM s
toGymShape (Infinite r) = DiscreteInf r
toGymShape Dummy = DummyShape

encodeLocations :: (Finitary name, Finitary r, ToJSON r, ToJSONKey name, ToJSONKey r) => Locations name r -> Value
encodeLocations locs = toJSON $ (fmap toGymShape) . fmap encodeLocationShape . FT.reifyFn $ locs

encodeLocationShape :: (Finitary r, ToJSON r) => LocationShape r -> LocationShape Int
encodeLocationShape (Deck seq') = Deck ((fromIntegral . toFinite <$> seq' :: Seq Int))
encodeLocationShape (Pile mri) = Pile (((M.mapKeys (fromIntegral . toFinite) mri) :: Map Int Int))
encodeLocationShape (Slot s) = Slot (((fromIntegral . toFinite <$> s) :: Maybe Int))
encodeLocationShape (Infinite inf) = Infinite (((fromIntegral . toFinite $ inf) :: Int))
encodeLocationShape Dummy = Dummy

fromGymShape (Sequence s) = Deck s
fromGymShape (MultiDiscrete p) = Pile p
fromGymShape (DiscreteM s) = Slot s
fromGymShape (DiscreteInf r) = Infinite r
fromGymShape DummyShape = Dummy

decodeLocationShape :: (Finitary r, Ord r) => LocationShape Int -> LocationShape r
decodeLocationShape (Deck seq) = Deck (fromFinite . fromIntegral <$> seq)
decodeLocationShape (Pile mri) = Pile (((M.mapKeys (fromFinite . fromIntegral) mri)))
decodeLocationShape (Slot (Just s)) = Slot (Just (fromFinite . fromIntegral $ s))
decodeLocationShape (Slot Nothing) = Slot Nothing
decodeLocationShape (Infinite inf) = Infinite (fromFinite . fromIntegral $ inf)
decodeLocationShape Dummy = Dummy

decodeLocations :: forall name r. (Ord r, Ord name, FromJSONKey name, FromJSONKey r, FromJSON r, Finitary r, Finitary name) => Text -> Locations name r
decodeLocations val = FT.unsafeUnreify . fmap decodeLocationShape . fmap (fromGymShape) . fromJust $ (decodeStrict . T.encodeUtf8 $ val :: Maybe (Map name GymFundShape))

-- instance (Finitary names, Finitary r, ToJSON r, ToJSONKey names, ToJSONKey r) => ToJSON (Locations names r) where
-- toJSON = encodeLocations

-- instance (Eq r, Finitary names, Finitary r) => Finitary (Locations names r)

-- Transfer should not happen unless sender and recipient allow it.
-- This enforces the invariant that resources cannot 'disappear.' Either
-- they will stay with the sender or they get transferred.
--
data TransferStatus = Success | Failure deriving (Eq, Ord, Show, Generic)

-- TODO: should be abstractable
moveFromL :: (Ord r) => r -> LocationShape r -> (LocationShape r, TransferStatus)
moveFromL r (Deck s) = case Seq.elemIndexL r s of -- search from left (i.e. "top")
  Nothing -> (Deck s, Failure)
  Just i -> (Deck (Seq.deleteAt i s), Success)
moveFromL r (Pile pileMap) = case M.lookup r pileMap of
  Nothing -> (Pile pileMap, Failure)
  Just i ->
    if i > 0
      then (Pile $ M.adjust (subtract 1) r pileMap, Success)
      else (Pile pileMap, Failure)
moveFromL _ (Slot Nothing) = (Slot Nothing, Failure)
moveFromL r (Slot (Just r')) =
  if r' == r
    then (Slot Nothing, Success)
    else (Slot (Just r'), Failure)
moveFromL r (Infinite r') =
  if r' == r
    then (Infinite r, Success)
    else (Infinite r, Failure)
moveFromL _ Dummy = (Dummy, Failure)

moveToL :: (Ord r) => r -> LocationShape r -> (LocationShape r, TransferStatus)
moveToL r (Deck s) = (Deck (r <| s), Success) -- add to left (i.e. "top")
moveToL r (Pile pileMap) = (Pile (M.alter addOneWithDefault r pileMap), Success)
  where
    addOneWithDefault Nothing = Just 1
    addOneWithDefault (Just i) = Just (i + 1)
-- if r `M.member` pileMap
-- then (Pile (M.adjust (+1) r pileMap), Success)
-- else (Pile pileMap, Failure)
moveToL r (Slot Nothing) = (Slot (Just r), Success)
moveToL _ (Slot (Just r')) = (Slot (Just r'), Failure)
moveToL _ Dummy = (Dummy, Failure)
moveToL r (Infinite r') =
  if r == r'
    then (Infinite r', Success)
    else (Infinite r', Failure)

-- laws!
-- The difficulty with source == target comes from thinking imperatively: "Move from here and to here"
-- OTOH that is the plain meaning of "transfer". Arguable what
-- "transfer from a deck to itself" means. Probably has to mean "move
-- topmost r to the top of the deck" to preserve laws.
--
-- Actually, can't even meaningfully test shapes for equality! Player A
-- and Player B might start with the same set of resources! Check has to
-- happen below.
transfer' :: (Ord r) => r -> LocationShape r -> LocationShape r -> (LocationShape r -> LocationShape r, LocationShape r -> LocationShape r, TransferStatus)
transfer' r source target =
  let (_, sourceStatus) = moveFromL r source
      (_, targetStatus) = moveToL r target
   in if sourceStatus == Failure || targetStatus == Failure
        then (id, id, Failure)
        else (fst . moveFromL r, fst . moveToL r, Success)

-- if name0 == name1 then loc0 == loc1
-- I think this works if we just swap the order of the two `update`s.
-- If we've already checked for success, then the order of those doesn't
-- matter in most situations. It's like -1 to one pile and +1 to
-- another. But if source == target, then we have to remove the thing
-- before adding it.
{-# NOINLINE transferSafelyByName #-}
transferSafelyByName :: (Ord name, Ord r) => r -> name -> name -> Locations name r -> (Locations name r, TransferStatus)
transferSafelyByName r name0 name1 locs =
  let loc0 = locs !!! name0
      loc1 = locs !!! name1
      (sourceF, targetF, mayber) = transfer' r loc0 loc1
   in case mayber of
        Failure -> (locs, Failure)
        Success -> (FT.applyAt name1 targetF . FT.applyAt name0 sourceF $ locs, Success)

transfer :: (Ord name, Ord r) => r -> name -> name -> Locations name r -> Locations name r
transfer r n0 n1 l = fst (transferSafelyByName r n0 n1 l)

-- Some games have a "swap" action in which r0 and r1 trade places
-- Suppose r0 is in slot0 and r1 is in slot1 -- how can they swap?
-- Swap always succeeds at least
-- TODO: is there a good way to do "swap all"?
swap :: (Ord r, Eq name) => r -> r -> name -> name -> Locations name r -> Locations name r
swap r0 r1 l0 l1 locs =
  let loc0 = locs !!! l0
      loc1 = locs !!! l1

      loc0' = fst . moveToL r1 . fst . moveFromL r0 $ loc0
      loc1' = fst . moveToL r0 . fst . moveFromL r1 $ loc1
   in FT.update (l0, loc0') . FT.update (l1, loc1') $ locs

----- Querying
histogram :: (Foldable f, Ord a) => f a -> (Map a) Int
histogram = foldl' (flip (M.alter plusOrInsertOne)) mempty
  where
    plusOrInsertOne = Just . maybe 1 (+ 1)

-- why not inventory = histogramF :(
inventory :: (Ord r) => LocationShape r -> (Map r) Int
inventory (Pile s) = s
inventory (Deck s) = histogram s
inventory (Slot Nothing) = M.empty
inventory (Slot (Just r)) = M.singleton r 1
inventory Dummy = M.empty
inventory (Infinite r) = M.singleton r maxBound

howManyF :: (Ord r) => LocationShape r -> (r -> Bool) -> Int
howManyF loc filt = sum . M.filterWithKey (\k _ -> filt k) . inventory $ loc

howMany' :: (Ord r) => LocationShape r -> r -> Int
howMany' loc res = fromMaybe 0 . M.lookup res . inventory $ loc

howMany :: (Ord r) => Locations l r -> l -> r -> Int
howMany locs lname = howMany' (locs !!! lname)

has' :: (Ord r) => LocationShape r -> r -> Bool
has' loc r = howMany' loc r > 0

findResourceWithin :: (Ord r) => r -> [n] -> Locations n r -> [n]
findResourceWithin res names locs = filter (\n -> locs !!! n `has'` res) names

listAll :: (Ord r) => n -> Locations n r -> [r]
listAll n locs = listAllF n locs (const True)

listAllShapeF :: (Ord a) => LocationShape a -> (a -> Bool) -> [a]
listAllShapeF locs filt = filter filt . M.keys . M.filter (> 0) . inventory $ locs

-- listAllShapeMapF :: Ord t => LocationShape t -> (t -> Bool) -> Map t Int
-- listAllShapeMapF locs filt = M.filterWithKey (\k _ -> filt k) . M.filter (>0) . inventory $ locs

listAllShape :: (Ord a) => LocationShape a -> [a]
listAllShape locs = listAllShapeF locs (const True)

listAllF :: (Ord r) => n -> Locations n r -> (r -> Bool) -> [r]
listAllF n locs = listAllShapeF (locs !!! n)

-- listAllF n locs filt = filter filt . M.keys . M.filter (>0) . inventory $ locs !!! n

peek' :: LocationShape r -> Maybe r
peek' (Pile s) = listToMaybe (M.keys . M.filter (> 0) $ s)
peek' (Deck (x :<| _)) = Just x
peek' (Deck Empty) = Nothing
peek' (Slot s) = s
peek' Dummy = Nothing
peek' (Infinite r) = Just r

---- Counters

data Counter = Counter
  { val :: Int,
    bounds :: (Int, Int)
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

makeFields ''Counter

encodeCounter :: Counter -> Value
encodeCounter (Counter val bounds) = toJSON $ Discrete bounds val

decodeCounter :: Text -> Counter
decodeCounter t = case (fromJust (decodeStrict . T.encodeUtf8 $ t) :: GymFundShape) of
  Discrete bounds val -> Counter val bounds

type Counters name = FTMap name Counter

encodeCounters :: (Finitary name, Ord name, ToJSON name, ToJSONKey name) => Counters name -> Value
encodeCounters = toJSON . fmap encodeCounter

makeCounter :: (Int, Int) -> Counter
makeCounter (a, b) = Counter a (a, b)

dummyCounter :: Counter
dummyCounter = Counter 0 (0, 0)

d6 :: Counter
d6 = makeCounter (1, 6)

mapCounter :: (Int -> Int) -> Counter -> (Counter, Maybe Int)
mapCounter f c@(Counter a (bl, bu)) =
  if f a >= bl && f a <= bu
    then (Counter (f a) (bl, bu), Just (f a))
    else (c, Nothing)

setCounter :: Counter -> Int -> Counter
setCounter c a = set #val a c

increment' :: Counter -> (Counter, Maybe Int)
increment' = mapCounter (+ 1)

increment :: Counter -> Counter
increment = fst . increment'

decrement' :: Counter -> (Counter, Maybe Int)
decrement' = mapCounter (subtract 1)

decrement :: Counter -> Counter
decrement = fst . decrement'

transferCounter' :: Counter -> Counter -> (Counter, Counter, TransferStatus)
transferCounter' fromc toc = case (decrement' fromc, increment' toc) of
  ((_, Nothing), _) -> (fromc, toc, Failure)
  (_, (_, Nothing)) -> (fromc, toc, Failure)
  ((fromc', _), (toc', _)) -> (fromc', toc', Success)

transferCounter :: (Eq cn) => cn -> cn -> Counters cn -> Counters cn
transferCounter fromcn tocn counters =
  let fromc = counters !!! fromcn
      toc = counters !!! tocn
   in case transferCounter' fromc toc of
        (_, _, Failure) -> counters
        (fromc', toc', Success) -> FT.update (fromcn, fromc') . FT.update (tocn, toc') $ counters

data GameObjects n cn r = GameObjects
  { locations :: Locations n r,
    counters :: Counters cn
  }
  deriving (Generic, FromJSON, ToJSON)

makeFields ''GameObjects

-- ---- Gym / training interface types ----

data GymSpace
  = GymDiscrete Int
  | GymBox Float Float [Int]
  | GymMultiBinary Int
  | GymMultiDiscrete [Int]
  | GymSequence GymSpace
  | GymDict [(Text, GymSpace)]
  deriving (Show, Generic)

instance ToJSON GymSpace where
  toJSON (GymDiscrete n)       = object ["type" .= ("Discrete" :: Text), "n" .= n]
  toJSON (GymBox lo hi shape)  = object ["type" .= ("Box" :: Text), "low" .= lo, "high" .= hi, "shape" .= shape]
  toJSON (GymMultiBinary n)    = object ["type" .= ("MultiBinary" :: Text), "n" .= n]
  toJSON (GymMultiDiscrete nv) = object ["type" .= ("MultiDiscrete" :: Text), "nvec" .= nv]
  toJSON (GymSequence s)       = object ["type" .= ("Sequence" :: Text), "space" .= s]
  toJSON (GymDict pairs)       = object ["type" .= ("Dict" :: Text), "spaces" .= M.fromList pairs]

infiniteUpperBound :: Int
infiniteUpperBound = 1000

locationShapeSpace :: forall r. (Finitary r) => LocationShape r -> GymSpace
locationShapeSpace (Slot _)     = GymDiscrete (n + 1)
  where n = length (inhabitants @r)
locationShapeSpace (Pile _)     = GymMultiDiscrete (replicate n n)
  where n = length (inhabitants @r)
locationShapeSpace (Deck _)     = GymSequence (GymDiscrete n)
  where n = length (inhabitants @r)
locationShapeSpace (Infinite _) = GymDiscrete infiniteUpperBound
locationShapeSpace Dummy        = GymDiscrete 1

counterSpace :: Counter -> GymSpace
counterSpace (Counter _ (lo, hi)) = GymBox (fromIntegral lo) (fromIntegral hi) [1]

gameObjectsSpace
  :: forall l cn r. (Finitary l, Finitary cn, Finitary r, Show l, Show cn)
  => GameObjects l cn r -> GymSpace
gameObjectsSpace (GameObjects locs cns) = GymDict $
  [(pack (show l), locationShapeSpace (locs !!! l)) | l <- inhabitants @l]
  ++ [(pack (show cn), counterSpace (cns !!! cn)) | cn <- inhabitants @cn]

encodeLocationObs :: forall r. (Finitary r, Ord r) => Maybe (LocationShape r) -> Value
encodeLocationObs Nothing                = toJSON (Nothing :: Maybe ())
encodeLocationObs (Just (Slot Nothing))  = toJSON (0 :: Int)
encodeLocationObs (Just (Slot (Just r))) = toJSON (fromIntegral (toFinite r) + 1 :: Int)
encodeLocationObs (Just (Pile m))        = toJSON [M.findWithDefault 0 r m | r <- inhabitants @r]
encodeLocationObs (Just (Deck s))        = toJSON [fromIntegral (toFinite r) :: Int | r <- foldr (:) [] s]
encodeLocationObs (Just (Infinite _))    = toJSON [infiniteUpperBound :: Int]
encodeLocationObs (Just Dummy)           = toJSON (0 :: Int)

encodeCounterObs :: Maybe Counter -> Value
encodeCounterObs Nothing              = toJSON [0 :: Int]
encodeCounterObs (Just (Counter v _)) = toJSON [v]
