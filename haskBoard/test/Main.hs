{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
module Main where
import Test.Tasty

import TestCount ( testsCount )
import TestFinitaryMap (testsFinitaryMap)

tests :: TestTree
tests = testGroup "Tests" [testsCount, testsFinitaryMap]


main :: IO ()
main = defaultMain tests
