{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module TestLocation
    where
import Data.Finitary (Finitary, inhabitants)
import GHC.Generics (Generic)
import qualified Test.QuickCheck as QC
import Location (LocationShape (..), transfer, inventory, Locations, howMany', howMany)
import qualified Data.Map as M
import FinitaryMap
import Test.Tasty.QuickCheck ((==>), Arbitrary (..))
import qualified Test.Tasty.QuickCheck as QC
import Count (Cnt (..))
import Test.Tasty (TestTree, testGroup)

data Resource = Wood | Gold | Card deriving (Eq, Ord, Show, Generic)

deriving instance Finitary Resource
deriving instance QC.Function Resource
instance QC.CoArbitrary Resource
instance QC.Arbitrary Resource where
    arbitrary = QC.elements inhabitants

data LocationNames = MyStuff | YourStuff | BoxTop deriving (Eq, Ord, Show, Generic)

deriving instance Finitary LocationNames
deriving instance QC.Function LocationNames
instance QC.CoArbitrary LocationNames
instance QC.Arbitrary  LocationNames where
    arbitrary = QC.elements inhabitants


instance (QC.Function a, QC.CoArbitrary a, QC.Arbitrary b) => QC.Arbitrary (FTMap a b) where
        arbitrary = fmap (FTMap . QC.applyFun) arbitrary

instance Arbitrary a => Arbitrary (Cnt a) where
    arbitrary = QC.frequency [(1, Cnt <$> arbitrary), (49, return Infinity)]

instance (Arbitrary r, Ord r) => QC.Arbitrary (LocationShape r) where
    arbitrary = QC.oneof
        [ Deck <$> arbitrary,
          Pile <$> arbitrary,
          Slot <$> arbitrary]

transfer_finite_histogram_successful :: Resource -> LocationNames -> LocationNames -> Locations LocationNames Resource -> QC.Property
transfer_finite_histogram_successful r l0 l1 locs = let
    locs' = transfer r l0 l1 locs
    -- TODO: need test for `howMany`'
    l0_result = howMany locs l0 r
    l1_result = howMany locs l1 r
    l0_result' = howMany locs' l0 r
    l1_result' = howMany locs' l1 r
   in ((l0_result < Infinity) && (l1_result < Infinity)) ==>
       ((
       (l0_result - l0_result' == 1)
       &&
           (l1_result - l1_result' == (-1))
        )
       ||
      (
       (l0_result - l0_result' == 0)
       &&
           (l1_result - l1_result' == 0)
      ))

prop_transfer_finite_histogram_succesful :: TestTree
prop_transfer_finite_histogram_succesful = QC.testProperty "transfer works right on finite histograms" $ \r l0 l1 locs -> transfer_finite_histogram_successful r l0 l1 locs

testsLocation :: TestTree
testsLocation = testGroup "Test: Location" [prop_transfer_finite_histogram_succesful]
