{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Options where

import Control.Lens (makeFields)
import Control.Monad (filterM, join)
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
import qualified Data.Set.NonEmpty as NES

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

youMayOnly :: (Ord pl, Monad f) => Player -> pl -> f (Options pl)
youMayOnly p play = youMay p (return $ NES.singleton play)

youMayNot' :: Ord pl => [pl] -> Options pl -> Maybe (Options pl)
youMayNot' pls (Options opls p) =
  fmap (`Options` p) . fromSetMaybe $ NESet.filter (`notElem` pls) opls

youMayNot :: (Functor f, Ord pl) => [pl] -> f (Options pl) -> f (Maybe (Options pl))
youMayNot pls = fmap (youMayNot' pls)

-- filter out plays IF THE CONDITION IS TRUE; returns Nothing if all plays filtered out
exceptIf :: (Monad m, Ord pl) => (pl -> m Bool) -> pl -> m (Options pl) -> m (Options pl)
exceptIf mfilt def mopts = mopts >>=  (exceptIf' mfilt def )

exceptIf' :: (Monad m, Ord pl) => (pl -> m Bool) -> pl -> Options pl -> m (Options pl)
exceptIf' mfilt def (Options plays player) = do
  filteredPlays <- filterM mfilt (NE.toList . NES.toList $ plays)
  case NE.nonEmpty filteredPlays of
    Nothing -> return (Options (NES.singleton def) player)
    Just ps -> return (Options (NES.fromList ps) player)

composer :: (Functor f, Monad m) => ( f a -> m (Maybe (f a))) -> (f a -> m (Maybe (f a))) ->  Maybe (f a) -> m (Maybe (f a))
composer g h xs = join $ fmap (fmap join . traverse h) (fmap join $ traverse g xs )
   

help :: (pl -> pl -> Bool) -> [pl] -> [pl]
help comparer list =
  let filters x = all (`comparer` x) list
   in filter filters list

helpM :: (Applicative m) => (pl -> pl -> m Bool) -> [pl] -> m [pl]
helpM mcomparer list = do
  let filters x = and <$> traverse (`mcomparer` x) list
  filterM filters list


replaceNonEmpty :: Ord a => a -> a -> NES.NESet a -> NES.NESet a
replaceNonEmpty del add = NES.insertSet add . NES.delete del

-- mcomparer a b = if true then remove b
unlessYouCould :: (Ord pl, Monad m) => (pl -> pl -> m Bool) -> pl -> m (Options pl) -> m (Options pl)
unlessYouCould mcomparer def mopts = do
  Options legal p <- mopts
  newLegal <- helpM mcomparer (NE.toList . NES.toList $ legal)
  -- TODO: improve
  case newLegal of
    [] -> return (Options (NES.singleton def) p)
    newLegal' -> return (Options (NES.fromList . NE.fromList $ newLegal') p)

-- Sparse list of legal action indices (0-indexed by Finitary ordering)
legalActionIndices :: forall pl. (Finitary pl) => Options pl -> [Int]
legalActionIndices (Options legal' _) = fromIntegral . toFinite <$> foldr (:) [] legal'

-- Total number of actions in the action space
actionSpaceSize :: forall pl. (Finitary pl) => Proxy pl -> Int
actionSpaceSize _ = length (inhabitants @pl)

-- Decode an action index back to pl
decodeAction :: (Finitary pl) => Int -> pl
decodeAction i = fromFinite (fromIntegral i)

