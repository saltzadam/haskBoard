{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Options where

import Control.Lens (makeFields)
import Control.Monad (filterM)
import Data.Aeson (FromJSON, ToJSON)
import Data.Generics.Labels ()
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import GHC.Generics (Generic)
import Game.Player
import Util (buildSafeNonempty)

data Options pl = Options
  { legal :: NonEmpty pl,
    owner :: Player
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

makeFields ''Options

instance Semigroup (Options pl) where
  Options l o <> Options l' _ = Options (l <> l') o

youMay' :: Player -> NonEmpty pl -> Options pl
youMay' p basePlays = Options basePlays p

youMay :: (Functor f) => Player -> f (NonEmpty pl) -> f (Options pl)
youMay p mBasePlays = youMay' p <$> mBasePlays

-- TODO: either label this as unsafe or figure out safe behavior.
youMayNot' :: (Eq pl) => [pl] -> Options pl -> Options pl
youMayNot' pls (Options opls p) = Options (NE.fromList (filter (`notElem` pls) (NE.toList opls))) p

youMayNot :: (Functor f, Eq pl) => [pl] -> f (Options pl) -> f (Options pl)
youMayNot pls mBasePlays = youMayNot' pls <$> mBasePlays

-- filter out plays IF THE CONDITION IS TRUE
exceptIf :: (Monad m) => (pl -> m Bool) -> m pl -> m (Options pl) -> m (Options pl)
exceptIf mfilt mdefault mopts = do
  let exceptFilt m = not <$> mfilt m
  def <- mdefault
  Options plays player <- mopts
  filteredPlays <- filterM exceptFilt (NE.toList plays)
  return (Options (buildSafeNonempty filteredPlays def) player)

help :: (pl -> pl -> Bool) -> [pl] -> [pl]
help comparer list =
  let filters x = all (($ x) . comparer) list
   in filter filters list

helpM :: (Applicative m) => (pl -> pl -> m Bool) -> [pl] -> m [pl]
helpM mcomparer list = do
  let filters x = and <$> traverse (($ x) . mcomparer) list
  filterM filters list

-- mcomparer a b = if a is in mopts then remove b
unlessYouCould :: (Ord pl, Monad m) => (pl -> pl -> m Bool) -> m (Options pl) -> m (Options pl)
unlessYouCould mcomparer mopts = do
  Options legal p <- mopts
  newLegal <- helpM mcomparer (NE.toList legal)
  -- TODO: NE.fromList is partial — if mcomparer or mopts is bad, newLegal can be []. Needs proper error handling.
  return $ Options (NE.fromList newLegal) p
