module ShuffleRNG where

import Control.Applicative (Applicative (..))
import Effectful (Eff, (:>))
import Effectful.Crypto.RNG
import System.Random.Shuffle

shuffleRNG :: RNG :> es => [a] -> Eff es [a]
shuffleRNG elements
  | null elements = return []
  | otherwise = fmap (shuffle elements) (rseqM (length elements - 1))
  where
    rseqM :: RNG :> es => Int -> Eff es [Int]
    rseqM 0 = return []
    rseqM i = liftA2 (:) (randomR (0, i)) (rseqM (i - 1))
