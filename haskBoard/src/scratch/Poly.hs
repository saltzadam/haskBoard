{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Poly where

import Control.Applicative ((<|>))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State (MonadState (..), State, StateT (..), evalState, lift, modify)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Data
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Foldable (traverse_)
import Data.Has (Has (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybeToList)
import Data.Tree (Forest, Tree (..))
import GHC.Generics (Generic)
import Safe (readMay)

data Color = Red | Blue | Green deriving (Eq, Ord, Show, Generic, Data, Read)

data ColorChoice s c = None | One c | Two s c c deriving (Eq, Ord, Show, Generic, Data, Read, Functor, Foldable, Traversable)

castRun :: (Typeable c, Typeable d, Typeable a) => (a -> b) -> (c, d) -> Maybe b
castRun f (x, y) =
  (f <$> cast x)
    <|> (f <$> cast y)

dataGetter :: (IO Color, IO Int)
dataGetter = (read <$> getLine, read <$> getLine)

dataGetterSimple :: (Color, Int)
dataGetterSimple = (Red, 1)

data A a = A a a deriving (Eq, Ord, Show, Typeable, Data)

applyConstr :: Data a => Constr -> [Dynamic] -> Maybe a
applyConstr ctor args =
  let nextField :: forall d. Data d => ReaderT [Dynamic] Maybe d
      nextField = do
        as <- ask
        case as of
          [] -> lift Nothing
          (a : _) -> do
            case fromDynamic a of
              Nothing -> local tail nextField
              Just x -> return x
   in case runReaderT (fromConstrM nextField ctor) args of
        Just x -> Just x
        _ -> Nothing -- runtime type error or too few / too many arguments

applyConstrS :: (Read a, Data a) => Constr -> [String] -> Maybe a
applyConstrS ctor args =
  let nextFieldS :: forall a. (Data a, Read a) => ReaderT [String] Maybe a
      nextFieldS = do
        as <- ask
        case as of
          [] -> lift Nothing
          (a : _) -> do
            case readMay a of
              Nothing -> local tail nextFieldS
              Just x -> return x
   in case runReaderT (fromConstrM (nextFieldS ctor) args of
        Just x -> Just x
        _ -> Nothing -- runtime type error or too few / too many arguments

ioColor :: ColorChoice () () -> IO (ColorChoice () Color)
ioColor = traverse (\_ -> getter dataGetter)

fields :: (Data a, Typeable b) => a -> [b]
fields = gmapQr (++) [] (maybeToList . cast)

-- colorChoiceConstrs :: [Constr]
-- colorChoiceConstrs = dataTypeConstrs (dataTypeOf (Two Red Green)) -- not ideal but would it be an issue in application?

-- colorChoiceConstrReps :: [ConstrRep]
-- colorChoiceConstrReps = constrRep <$> colorChoiceConstrs

-- applyConstr :: Data a => Constr -> [Dynamic] -> Maybe a
-- applyConstr ctor args =
--  let nextField :: forall d. Data d => StateT [Dynamic] Maybe d
--      nextField = do
--        as <- get
--        case as of
--          [] -> lift Nothing -- too few arguments
--          (a : rest) -> do
--            put rest
--            case fromDynamic a of
--              Nothing -> lift Nothing -- runtime type mismatch
--              Just x -> return x
--   in case runStateT (fromConstrM nextField ctor) args of
--        Just (x, []) -> Just x
--        _ -> Nothing -- runtime type error or too few / too many arguments
--        --
--        -- class GettableFromIO a where
--        --   getFrom :: IO a

-- class Applicative f => ApN f a b | a b -> f where
--   apN :: f a -> b

-- instance (Applicative f, b ~ f a) => ApN f a b where
--   apN = id

-- instance {-# OVERLAPS #-} (Applicative f, ApN f a' b', b ~ (f a -> b')) => ApN f (a -> a') b where
--   apN f fa = apN (f <*> fa)

-- lift :: ApN f a b => a -> b
-- lift a = apN (pure a)

mkSingletonTree :: NonEmpty a -> Tree a
mkSingletonTree (a :| []) = Node a []
mkSingletonTree (a :| (b : as)) = Node a [mkSingletonTree (b :| as)]

mergeTree :: (Show a, Eq a) => Tree a -> Tree a
mergeTree (Node a as) = Node a (mergeForest as)

mergeForest :: (Show a, Eq a) => Forest a -> Forest a
mergeForest forest = mergeTree <$> go forest [] []
  where
    -- check if node can merge with anything in seen.
    -- if so, merge it, then move to nodes
    -- if not, add it to seen, then move to nodes
    -- go remainingNodes seen seenOfSeen
    go (node : nodes) [] seenOfSeen = go nodes (node : seenOfSeen) [] -- restart, move seenOfSeen to seen
    go [] seen seenOfSeen = seen ++ seenOfSeen -- finish
    go (node : nodes) (seen : restOfSeen) seenOfSeen = case mergeTreesMaybe node seen of
      Nothing -> go (node : nodes) restOfSeen (seen : seenOfSeen)
      Just merged -> go nodes ((merged : restOfSeen) ++ seenOfSeen) []

    mergeTreesMaybe :: Eq a => Tree a -> Tree a -> Maybe (Tree a)
    mergeTreesMaybe (Node a as) (Node b bs) =
      if a == b
        then Just (Node a (as ++ bs))
        else Nothing

-- TODO: add ConstrRep and DataRep here!

buildTreeOfChoices :: (Show a) => a -> Tree String
buildTreeOfChoices = mkSingletonTree . NE.fromList . words . show

ioChooser :: (a -> String) -> NonEmpty a -> IO a
ioChooser shower options = do
  traverse_ (putStrLn . shower) options
  picked <- getLine
  let picked' = read picked :: Int
  return (options NE.!! (picked' - 1))

ioChooseForest :: Forest String -> IO String
ioChooseForest forest = unwords <$> go forest
  where
    go :: Forest String -> IO [String]
    go [] = pure []
    go forest' = do
      Node str children <- ioChooser (\(Node a _) -> show a) (NE.fromList forest')
      (str :) <$> go children

chooseOptionIO :: (Show a, Read a) => [a] -> IO a
chooseOptionIO = fmap read . ioChooseForest . mergeForest . fmap buildTreeOfChoices
