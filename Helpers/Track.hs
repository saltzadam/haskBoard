{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Track where

import Control.Monad
import Data.Bifunctor (Bifunctor (..))
import Data.Finitary (Finitary)
import Data.Foldable (find)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import GHC.Generics (Generic)
import Game.GameAction (GameAction (..))
import Game.Location (LocationShape)
import Game.Player (Player)
import Game.Rules
import Helpers
import Util (ifM, safeIndexList)

-- TODO: rewrite stuff as folds

newtype Track l = Track {slots :: NonEmpty l} deriving (Functor, Applicative, Monad, Generic)

newtype PlayerTrack l = PlayerTrack (Player -> Track l) -- want some notion of one track for each player, but they're identical.

getTrack :: Track l -> NonEmpty l
getTrack (Track slots) = slots

start :: Track a -> a
start (Track slots) = NE.head slots

end :: Track a -> a
end (Track slots) = NE.last slots

startTrack :: r -> l -> Track l -> GameRule l cn r ph pl ()
startTrack piece currLoc (Track slots) = act (MkTransfer currLoc (NE.head slots) piece)

lookSlot :: (Ord r, Finitary l, Ord l) => Track l -> l -> Maybe (GameRule l cn r ph pl (LocationShape r))
lookSlot (Track slots) l = fmap lookLocation (find (== l) slots)

lookPosition :: (Eq l) => Track l -> Int -> Maybe (GameRule l cn r ph pl (LocationShape r))
lookPosition (Track slots) i =
  let enumSlots = zip [1 ..] (NE.toList slots)
   in fmap lookLocation (lookup i enumSlots)

-- 1 indexed!!
slotHeight :: forall l. (Eq l) => Track l -> l -> Maybe Int
slotHeight (Track slots) loc = go loc slots 1
  where
    go :: l -> NonEmpty l -> Int -> Maybe Int
    go loc (slot :| (next : rest)) i = if loc == slot then Just i else go loc (next :| rest) (i + 1)
    go loc (slot :| []) i = if loc == slot then Just i else Nothing

transferTo :: l -> Track l -> r -> GameRule l cn r ph pl ()
transferTo source targetTrack = transfer source (start targetTrack)

-- removes the highest instance of the resource
transferFrom :: (Ord r, Eq l) => Track l -> l -> r -> GameRule l cn r ph pl ()
transferFrom sourceTrack target res = do
  sourceSlot <- resMaxSlot res sourceTrack
  case sourceSlot of
    Nothing -> return ()
    Just sourceSlot' -> transfer sourceSlot' target res

data AdvanceException = AtTop | NotOnTrack | AtBottom deriving (Eq, Ord, Show, Generic)

-- advances bottommost
advance' :: forall r l cn ph pl. (Ord r, Eq l) => r -> Track l -> GameRule l cn r ph pl (Either AdvanceException (GameAction l cn r ph))
advance' res (Track slots) = go res slots
  where
    go :: r -> NonEmpty l -> GameRule l cn r ph pl (Either AdvanceException (GameAction l cn r ph))
    go res (top :| []) = ifM (top `has` res) (pure $ Left AtTop) (pure $ Left NotOnTrack)
    go res (slot :| (next : rest)) = ifM (slot `has` res) (pure . Right $ MkTransfer slot next res) (go res (next :| rest))

advance :: (Ord r, Eq l) => r -> Track l -> GameRule l cn r ph pl ()
advance res track = do
  result <- advance' res track
  case result of
    Left _ -> return ()
    Right action -> act action

advanceOrInsert :: (Ord r, Eq l) => r -> l -> Track l -> GameRule l cn r ph pl ()
advanceOrInsert res source track' = ifM (holds res track') (advance res track') (startTrack res source track')

advanceOrInsertAt :: (Ord r, Eq l) => r -> l -> Track l -> Int -> GameRule l cn r ph pl ()
advanceOrInsertAt res source track'@(Track slots) startingPosition = ifM (holds res track') (advance res track') $
  case safeIndexList startingPosition (NE.toList slots) of
    Nothing -> transfer source (NE.last slots) res
    Just slot -> transfer source slot res

recede' :: forall r l cn ph pl. (Ord r, Eq l) => r -> Track l -> GameRule l cn r ph pl (Either AdvanceException (GameAction l cn r ph))
recede' res (Track slots) = first reverseExceptions <$> advance' res (Track (NE.reverse slots))
  where
    reverseExceptions AtTop = AtBottom
    reverseExceptions AtBottom = AtTop
    reverseExceptions NotOnTrack = NotOnTrack

recede :: (Ord r, Eq l) => r -> Track l -> GameRule l cn r ph pl ()
recede res track = do
  result <- recede' res track
  case result of
    Left _ -> return ()
    Right action -> act action

count :: (Ord r, Eq l) => r -> Track l -> GameRule l cn r ph pl Int
count res (Track slots) = sum <$> traverse (`howManyAt` res) slots

holds :: (Ord r, Eq l) => r -> Track l -> GameRule l cn r ph pl Bool
holds res track' = (> 0) <$> count res track'

resMinSlot :: forall r l cn ph pl. (Ord r, Eq l) => r -> Track l -> GameRule l cn r ph pl (Maybe l)
resMinSlot res (Track slots) = go res slots
  where
    go :: r -> NonEmpty l -> GameRule l cn r ph pl (Maybe l)
    go r (slot :| []) = ifM (slot `has` r) (pure $ Just slot) (pure Nothing)
    go r (slot :| (next : rest)) = ifM (slot `has` r) (pure $ Just slot) (go r (next :| rest))

resMaxSlot :: (Ord r, Eq l) => r -> Track l -> GameRule l cn r ph pl (Maybe l)
resMaxSlot res (Track slots) = resMinSlot res (Track (NE.reverse slots))

resMinHeight :: (Ord r, Eq a) => r -> Track a -> GameRule a cn r ph pl (Maybe Int)
resMinHeight res track = (slotHeight track =<<) <$> resMinSlot res track

resMaxHeight :: (Ord r, Eq a) => r -> Track a -> GameRule a cn r ph pl (Maybe Int)
resMaxHeight res track = (slotHeight track =<<) <$> resMaxSlot res track

removeAll :: (Ord r, Eq l) => r -> Track l -> l -> GameRule l cn r ph pl ()
removeAll res track target = do
  num <- count res track
  replicateM_ num (transferFrom track target res)

resAtTop :: (Ord r, Eq l) => r -> Track l -> GameRule l cn r ph pl Bool
resAtTop r track = do
  rSlot <- resMaxSlot r track
  let height = slotHeight track =<< rSlot
  let maxHeight = slotHeight track (end track)
  return (height == maxHeight)
