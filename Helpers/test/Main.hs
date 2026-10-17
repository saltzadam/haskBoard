{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Data.Aeson (decode, encode)
import Data.Finitary (Finitary (..), inhabitants)
import Data.Finite (finite)
import Data.List (sort)
import Grid
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain (testGroup "Helpers" [cellTests, geometryTests])

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

geometryTests :: TestTree
geometryTests =
  testGroup
    "geometry"
    [ testCase "offset inside the board" $
        offset (1, 2) (c 0 0) @?= Just (c 1 2),
      testCase "offset off the top" $
        offset (-1, 0) (c 0 0) @?= Nothing,
      testCase "offset off the right" $
        offset (0, 4) (c 0 0) @?= Nothing,
      testCase "neighbors4 in the interior" $
        sort (neighbors4 (c 1 1)) @?= [c 0 1, c 1 0, c 1 2, c 2 1],
      testCase "neighbors4 in a corner" $
        sort (neighbors4 (c 0 0)) @?= [c 0 1, c 1 0],
      testCase "neighbors8 in the interior" $
        length (neighbors8 (c 1 1)) @?= 8,
      testCase "neighbors8 in a corner" $
        sort (neighbors8 (c 0 0)) @?= [c 0 1, c 1 0, c 1 1],
      testCase "Coord neighbors for Cell is 4-adjacency" $
        sort (neighbors (c 1 1)) @?= sort (neighbors4 (c 1 1))
    ]
