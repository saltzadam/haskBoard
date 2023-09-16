{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# HLINT ignore "Functor law" #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Menu where

import Control.Monad
import Data.Finitary (Finitary (..), inhabitants)
import Data.Kind (Type)
import Data.Set (Set)
import qualified GHC.Generics as GHC
import GHC.TypeLits (KnownNat, Nat, type (-), type (^))
import Generics.SOP
import Generics.SOP.Constraint (SListIN)
import Safe (readMay)

-- Goal is to take a list [adt] and "select" options one at a time.
-- idea: [adt] -> [generic rep] -> tree (generic rep) -> traverse tree

data Color = Red | Blue | Green deriving (Eq, Ord, Show, GHC.Generic, Read, Finitary)

data ColorChoice c = None | One c | Two c c | Three c c c deriving (Eq, Ord, Show, GHC.Generic, Read)

instance Generic Color

instance Generic (ColorChoice c)

-- toGen :: [ColorChoice c] -> [SOP I '[ '[], '[c], '[c, c], '[[Char], [c]]]]
-- toGen = fmap from

f :: forall a. (All2 GetUI (Code a), Generic a) => IO [a]
f = fmap (fmap to) . traverse hsequence' $ apInjs_POP (hcpure allg (Comp (fmap I getui)))

-- https://stackoverflow.com/questions/70824108/creating-a-sum-constructor-value-using-generics-sop

-- |
-- Infrastructure to create a single sum constructor given its type index and value.
--
-- - `mkSum @0 @(Code a) x` creates the first sum constructor;
-- - `mkSum @1 @(Code a) x` creates the second sum constructor;
-- - etc.
--
-- It is type-checked that the `x` here matches the type of nth constructor of `a`.
class MkSum (idx :: Nat) (xss :: [[Type]]) where
  mkSum :: NP I (IndexList idx xss) -> NS (NP I) xss

instance {-# OVERLAPPING #-} MkSum 0 (xs ': xss) where
  mkSum = Z

instance
  {-# OVERLAPPABLE #-}
  ( MkSum (idx - 1) xss,
    IndexList idx (xs ': xss) ~ IndexList (idx - 1) xss
  ) =>
  MkSum idx (xs ': xss)

-- | Indexing type-level lists
type family IndexList (n :: Nat) (l :: [k]) :: k where
  IndexList 0 (x ': _) = x
  IndexList n (x : xs) = IndexList (n - 1) xs

aColorChoice :: ColorChoice Color -- One Blue
aColorChoice = to . SOP $ mkSum @1 @(Code (ColorChoice Color)) (I Blue :* Nil)

g :: (AllN h GetUI xs, HSequence h, SListIN h xs, HPure h) => IO (h I xs)
g = hsequence' $ hcpure allg (Comp (fmap I getui))

h :: (All GetUI x0) => NS (NP IO) (x0 : xs)
h = Z (hcpure allg getui)

h' :: (All GetUI x, All (All Top) xs) => (IO (SOP I (x : xs)))
h' = hsequence (SOP h)

h'' :: forall a xs xss. (Generic a, Code a ~ (xs : xss), All GetUI xs) => IO a
h'' = fmap to h'

fit :: forall a xs xss. (Generic a, Code a ~ (xs : xss), All GetUI xs) => IO a
fit = fmap to . hsequence . SOP $ Z (hcpure allg getui)

fit0 :: (Generic a, Code a ~ (xs : xss), All GetUI xs, All (All GetUI) xss) => [IO a]
fit0 = fmap (fmap to) . fmap hsequence . apInjs_POP $ hcpure allg getui

class GetUI a where
  getui :: IO a

instance (Finitary a, Show a) => GetUI a where
  getui = selectFromFinitary

-- instance GetUI Color where
--   getui = pure Blue

-- instance GetUI String where
--   getui = pure "test"

-- instance GetUI Color => GetUI [Color] where
--   getui = fmap (: []) (getui :: IO Color)

selectFromFinitary :: forall a. (Finitary a, Show a) => IO a
selectFromFinitary = do
  let enumedChoices = zip [0 ..] inhabitants :: [(Int, a)]
  forM_ enumedChoices print
  c <- getLine
  case (readMay c :: Maybe Int) of
    Nothing -> print "not valid" >> selectFromFinitary :: IO a
    Just i -> return (inhabitants !! i)

allg :: Proxy GetUI
allg = Proxy
