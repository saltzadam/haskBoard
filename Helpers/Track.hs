{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Track where

import Control.Monad
import Data.Bifunctor (Bifunctor (..))
import Data.Finitary (Finitary)
import Data.Foldable (find)
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (Semigroup (..), mtimesDefault)
import GHC.Generics (Generic)
import Game.GameNode (GameAction (..), GameNode)
import Game.Location (LocationShape)
import Game.Monad (GameEff)
import Game.Player (Player)
import Helpers
import Util (ifM, pureIfM)

-- data Capacity l = Capacity [l] deriving (Generic)
-- data SingleTokenCapacity l = SingleTokenCapacity l deriving (Generic)
-- TODO: rewrite stuff as folds

newtype Track l = Track {slots :: NonEmpty l} deriving (Functor, Applicative, Monad, Generic)

newtype PlayerTrack l = PlayerTrack (Player -> Track l) -- want some notion of one track for each player, but they're identical.

getTrack :: Track l -> NonEmpty l
getTrack (Track slots) = slots

start :: Track a -> a
start (Track slots) = NE.head slots

end :: Track a -> a
end (Track slots) = NE.last slots

startTrack :: r -> l -> Track l -> GameEff l cn r ph pl i [GameNode l cn r ph pl i]
startTrack piece currLoc (Track slots) = return [mkTransfer currLoc (NE.head slots) piece]

lookSlot :: (Ord r, Finitary l, Ord l) => Track l -> l -> Maybe (GameEff l cn r ph pl i (LocationShape r))
lookSlot (Track slots) l = fmap lookLocation (find (== l) slots)

lookPosition :: Eq l => Track l -> Int -> Maybe (GameEff l cn r ph pl i (LocationShape r))
lookPosition (Track slots) i =
  let enumSlots = zip [1 ..] (NE.toList slots)
   in fmap lookLocation (lookup i enumSlots)

-- 1 indexed!!
slotHeight :: forall l. Eq l => Track l -> l -> Maybe Int
slotHeight (Track slots) loc = go loc slots 1
  where
    go :: l -> NonEmpty l -> Int -> Maybe Int
    go loc (slot :| (next : rest)) i = if loc == slot then Just i else go loc (next :| rest) (i + 1)
    go loc (slot :| []) i = if loc == slot then Just i else Nothing

transferTo :: l -> Track l -> r -> GameEff l cn r ph pl i [GameNode l cn r ph pl i]
transferTo source targetTrack = justTransfer source (start targetTrack)

-- removes the highest instance of the resource
transferFrom :: (Ord r, Eq l) => Track l -> l -> r -> GameEff l cn r ph pl i [GameNode l cn r ph pl i]
transferFrom sourceTrack target res = do
  sourceSlot <- resMaxSlot res sourceTrack
  case sourceSlot of
    Nothing -> return justDoNothing
    Just sourceSlot' -> justTransfer sourceSlot' target res

data AdvanceException = AtTop | NotOnTrack | AtBottom deriving (Eq, Ord, Show, Generic)

-- advances bottommost
advance' :: forall r l cn ph pl i. (Ord r, Eq l) => r -> Track l -> GameEff l cn r ph pl i (Either AdvanceException (GameNode l cn r ph pl i))
advance' res (Track slots) = go res slots
  where
    go :: r -> NonEmpty l -> GameEff l cn r ph pl i (Either AdvanceException (GameNode l cn r ph pl i))
    go res (top :| []) = pureIfM (top `has` res) (Left AtTop) (Left NotOnTrack)
    go res (slot :| (next : rest)) = ifM (slot `has` res) (pure . Right $ mkTransfer slot next res) (go res (next :| rest))

advance :: (Ord r, Eq l) => r -> Track l -> GameEff l cn r ph pl i [GameNode l cn r ph pl i]
advance res track = F.toList <$> advance' res track

advanceOrInsert :: (Ord r, Eq l) => r -> l -> Track l -> GameEff l cn r ph pl i [GameNode l cn r ph pl i]
advanceOrInsert res source track' = ifM (holds res track') (advance res track') (startTrack res source track')

recede' :: forall r l cn ph pl i. (Ord r, Eq l) => r -> Track l -> GameEff l cn r ph pl i (Either AdvanceException (GameNode l cn r ph pl i))
recede' res (Track slots) = first reverseExceptions <$> advance' res (Track (NE.reverse slots))
  where
    reverseExceptions AtTop = AtBottom
    reverseExceptions AtBottom = AtTop
    reverseExceptions NotOnTrack = NotOnTrack

recede :: (Ord r, Eq l) => r -> Track l -> GameEff l cn r ph pl i [GameNode l cn r ph pl i]
recede res track = F.toList <$> recede' res track

count :: (Ord r, Eq l) => r -> Track l -> GameEff l cn r ph pl i Int
count res (Track slots) = sum <$> traverse (`howManyAt` res) slots

holds :: (Ord r, Eq l) => r -> Track l -> GameEff l cn r ph pl i Bool
holds res track' = (> 0) <$> count res track'

resMinSlot :: forall r l cn ph pl i. (Ord r, Eq l) => r -> Track l -> GameEff l cn r ph pl i (Maybe l)
resMinSlot res (Track slots) = go res slots
  where
    go :: r -> NonEmpty l -> GameEff l cn r ph pl i (Maybe l)
    go r (slot :| []) = ifM (slot `has` r) (pure $ Just slot) (pure Nothing)
    go r (slot :| (next : rest)) = ifM (slot `has` r) (pure $ Just slot) (go r (next :| rest))

resMaxSlot :: (Ord r, Eq l) => r -> Track l -> GameEff l cn r ph pl i (Maybe l)
resMaxSlot res (Track slots) = resMinSlot res (Track (NE.reverse slots))

resMinHeight :: (Ord r, Eq a) => r -> Track a -> GameEff a cn r ph pl i (Maybe Int)
resMinHeight res track = (slotHeight track =<<) <$> resMinSlot res track

resMaxHeight :: (Ord r, Eq a) => r -> Track a -> GameEff a cn r ph pl i (Maybe Int)
resMaxHeight res track = (slotHeight track =<<) <$> resMaxSlot res track

removeAll :: (Ord r, Eq l) => r -> Track l -> l -> GameEff l cn r ph pl i [GameNode l cn r ph pl i]
removeAll res track target = mtimesDefault <$> count res track <*> transferFrom track target res

resAtTop :: (Ord r, Eq l) => r -> Track l -> GameEff l cn r ph pl i Bool
resAtTop r track = do
  rSlot <- resMaxSlot r track
  let height = slotHeight track =<< rSlot
  let maxHeight = slotHeight track (end track)
  return (height == maxHeight)
