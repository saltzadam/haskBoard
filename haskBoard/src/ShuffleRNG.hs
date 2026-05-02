module ShuffleRNG where

import Effectful (Eff, (:>))
import Effectful.Crypto.RNG
import System.Random.Shuffle

shuffleRNG :: (RNG :> es) => [a] -> Eff es [a]
shuffleRNG elements
  | null elements = return []
  | otherwise = do
      fmap (shuffle elements) (rseqM (length elements - 1))
  where
    rseqM :: (RNG :> es) => Int -> Eff es [Int]
    rseqM n = traverse (\i -> randomR (0, i)) [n, n-1 .. 1]
