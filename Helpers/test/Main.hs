{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Data.Aeson (decode, encode)
import Data.Finitary (Finitary (..), inhabitants)
import Data.Finite (finite)
import Grid
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain (testGroup "Helpers" [cellTests])

-- Fixture: a 3-row, 4-column board.

type C = Cell 3 4

c :: Integer -> Integer -> C
c r col = Cell (finite r) (finite col)

cellTests :: TestTree
cellTests =
  testGroup
    "Cell"
    [ testCase "has rows * cols inhabitants" $
        length (inhabitants @C) @?= 12,
      testCase "inhabitants are strictly ascending (reifyFn relies on this)" $
        let xs = inhabitants @C in and (zipWith (<) xs (drop 1 xs)) @? "not ascending",
      testCase "fromFinite . toFinite = id" $
        let xs = inhabitants @C in map (fromFinite . toFinite) xs @?= xs,
      testCase "JSON round trip" $
        decode (encode (c 2 3)) @?= Just (c 2 3),
      testCase "JSON rejects out-of-bounds cells" $
        decode (encode ((3, 0) :: (Integer, Integer))) @?= (Nothing :: Maybe C)
    ]
