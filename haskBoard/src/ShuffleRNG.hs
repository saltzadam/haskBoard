module ShuffleRNG where

import qualified Debug.Trace as Debug
import Effectful (Eff, (:>))
import Effectful.Crypto.RNG
import System.Random.Shuffle

shuffleRNG :: (RNG :> es) => [a] -> Eff es [a]
shuffleRNG elements
  | null elements = Debug.trace "empty" $ return []
  | otherwise = do
      fmap (shuffle elements) (rseqM (length elements - 1))
  where
    rseqM :: (RNG :> es) => Int -> Eff es [Int]
    rseqM 0 = return []
    rseqM i = liftA2 (:) (randomR (0, i)) (rseqM (i - 1))
