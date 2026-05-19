{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Options where

import Control.Lens (makeFields)
import Control.Monad (filterM)
import Data.Aeson (FromJSON, ToJSON)
import Data.Finitary (Finitary (..), inhabitants, toFinite, fromFinite)
import Data.Generics.Labels ()
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import GHC.Generics (Generic)
import Game.Player

-- | Convert a Set to Maybe NESet (Nothing if empty).
fromSetMaybe :: Ord a => Set a -> Maybe (NESet a)
fromSetMaybe s = NESet.fromList <$> NE.nonEmpty (S.toList s)

data Options pl = Options
  { legal :: NESet pl,
    owner :: Player
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

makeFields ''Options

instance Ord pl => Semigroup (Options pl) where
  Options l o <> Options l' _ = Options (l <> l') o

youMay' :: Ord pl => Player -> NESet pl -> Options pl
youMay' p basePlays = Options basePlays p

youMay :: (Functor f, Ord pl) => Player -> f (NESet pl) -> f (Options pl)
youMay p mBasePlays = youMay' p <$> mBasePlays

youMayNot' :: Ord pl => [pl] -> Options pl -> Maybe (Options pl)
youMayNot' pls (Options opls p) =
  fmap (`Options` p) . fromSetMaybe $ NESet.filter (`notElem` pls) opls

youMayNot :: (Functor f, Ord pl) => [pl] -> f (Options pl) -> f (Maybe (Options pl))
youMayNot pls = fmap (youMayNot' pls)

-- filter out plays IF THE CONDITION IS TRUE; returns Nothing if all plays filtered out
exceptIf :: (Monad m, Ord pl) => (pl -> m Bool) -> m (Options pl) -> m (Maybe (Options pl))
exceptIf mfilt mopts = do
  Options plays player <- mopts
  kept <- fromSetMaybe . S.fromList <$> filterM (fmap not . mfilt) (foldr (:) [] plays)
  return (fmap (`Options` player) kept)

help :: (pl -> pl -> Bool) -> [pl] -> [pl]
help comparer list =
  let filters x = all (($ x) . comparer) list
   in filter filters list

helpM :: (Applicative m) => (pl -> pl -> m Bool) -> [pl] -> m [pl]
helpM mcomparer list = do
  let filters x = and <$> traverse (($ x) . mcomparer) list
  filterM filters list

-- mcomparer a b = if a is in mopts then remove b
unlessYouCould :: (Ord pl, Monad m) => (pl -> pl -> m Bool) -> m (Options pl) -> m (Maybe (Options pl))
unlessYouCould mcomparer mopts = do
  Options legal p <- mopts
  newLegal <- fromSetMaybe . S.fromList <$> helpM mcomparer (foldr (:) [] legal)
  return (fmap (`Options` p) newLegal)

-- Sparse list of legal action indices (0-indexed by Finitary ordering)
legalActionIndices :: forall pl. (Finitary pl) => Options pl -> [Int]
legalActionIndices (Options legal' _) = fromIntegral . toFinite <$> foldr (:) [] legal'

-- Total number of actions in the action space
actionSpaceSize :: forall pl. (Finitary pl) => Proxy pl -> Int
actionSpaceSize _ = length (inhabitants @pl)

-- Decode an action index back to pl
decodeAction :: (Finitary pl) => Int -> pl
decodeAction i = fromFinite (fromIntegral i)

