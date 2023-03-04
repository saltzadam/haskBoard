{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
module TestFinitaryMap where
import FinitaryMap
import Test.QuickCheck (Fun, applyFun, (==>))
import Data.Map (Map)
import Control.Lens (set, view)
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty (TestTree, testGroup)
import GHC.Generics (Generic)
import Data.Finitary (Finitary, inhabitants)
import qualified Data.Map as M

-- TODO: use SmallCheck or something
data SmallType = D0 | D1 | D2 | D3 | D4 | D5 deriving (Eq, Ord, Show, Generic)
deriving instance Finitary SmallType
deriving instance QC.Function SmallType
instance QC.CoArbitrary SmallType
instance QC.Arbitrary SmallType where
    arbitrary = QC.elements [D0, D1, D2, D3, D4, D5]

finitarymap_roundtrip :: Fun SmallType SmallType -> Bool
finitarymap_roundtrip xs = unsafeUnreify (reifyFn (FTMap (applyFun xs))) == FTMap (applyFun xs)

prop_finitarymap_roundtrip :: TestTree
prop_finitarymap_roundtrip = QC.testProperty "finitary map roundtrip" finitarymap_roundtrip

-- use Maybe Bool as three element set 
finitarymap_foldable_agrees :: Map SmallType Int -> QC.Property
finitarymap_foldable_agrees m = (M.keys m == inhabitants) ==> length (unsafeUnreify m) == length m

prop_finitarymap_foldable_agrees :: TestTree
prop_finitarymap_foldable_agrees = QC.testProperty "finitary map foldable (length)" finitarymap_foldable_agrees

finitarymap_update :: Fun SmallType SmallType -> (SmallType, SmallType) -> Bool
finitarymap_update xs (a,b) = update (a,b) (FTMap (applyFun xs)) !!! a == b

prop_finitarymap_update :: TestTree
prop_finitarymap_update = QC.testProperty "finitary map update" finitarymap_update

finitarymap_lens_update :: Fun SmallType SmallType -> (SmallType, SmallType) -> Bool
finitarymap_lens_update  xs (a,b) = let
    ft = FTMap (applyFun xs)
    in
        view (ftAt a) (set (ftAt a) b ft) == b


prop_finitarymap_lens_update :: TestTree
prop_finitarymap_lens_update = QC.testProperty "finitary map lens: update" finitarymap_lens_update

testsFinitaryMap :: TestTree
testsFinitaryMap = testGroup "FinitaryMap" [prop_finitarymap_roundtrip, prop_finitarymap_foldable_agrees, prop_finitarymap_update, prop_finitarymap_lens_update]
