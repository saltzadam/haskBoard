module TestCount
    where
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Count
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Map (Map)

prop_cnt_plus_num :: TestTree 
prop_cnt_plus_num = QC.testProperty "Add Cnt Ints" $ \i j -> Cnt (i :: Int) + Cnt j == Cnt (i+j)

prop_cnt_plus_inf_num :: TestTree
prop_cnt_plus_inf_num = QC.testProperty "Cnt Int + Infinity" $ \i -> Cnt (i :: Int) + Infinity == Infinity

prop_cnt_plus_num_inf :: TestTree
prop_cnt_plus_num_inf = QC.testProperty "Infinity + Cnt Int" $ \i -> Infinity + Cnt (i :: Int) == Infinity

prop_cnt_plus_inf_inf :: TestTree
prop_cnt_plus_inf_inf = QC.testProperty "Infinity + Infinity" $ \b -> const (Infinity + Infinity == Infinity) (b :: Bool) 


-- TODO: when defaultable-map is removed, can't use the applicative instance anymore :(
histogramFSemigroup :: (Eq a, Ord a, Foldable f) => f a -> f a -> Bool
histogramFSemigroup f0 f1 = histogramF (F.toList f0 <> F.toList f1)
    == ((+) <$> histogramF f0 <*> histogramF f1)

prop_histogramF_semigroup :: TestTree
prop_histogramF_semigroup = QC.testProperty "histogramF semigroup" $ \f0 f1 -> histogramFSemigroup f0 (f1 :: (Map Int Int))

-- prop_histogramF_monoid :: Bool
histogramFLength :: (Foldable f, Ord a) => f a -> Bool
histogramFLength f0 = Cnt (F.length f0) == sum (histogramF f0)

prop_histogramF_length :: TestTree
prop_histogramF_length = QC.testProperty "histogramF length" $ \f -> histogramFLength (f :: (Map Int Int))

testsCountCnt :: TestTree
testsCountCnt = testGroup "Count: Cnt" [prop_cnt_plus_num, prop_cnt_plus_inf_num, prop_cnt_plus_num_inf, prop_cnt_plus_inf_inf]

testsCountHistogram :: TestTree
testsCountHistogram = testGroup "Count: Histogram" [prop_histogramF_semigroup, prop_histogramF_length]

testsCount :: TestTree
testsCount = testGroup "Count" [testsCountCnt, testsCountHistogram]


