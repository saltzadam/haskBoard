{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
module Main where
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import TestCount

tests :: TestTree
tests = testGroup "Tests" [testsCount]


main :: IO ()
main = defaultMain tests
