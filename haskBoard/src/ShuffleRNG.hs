module ShuffleRNG (shuffleRNG, shuffleList) where

import Data.List (mapAccumL)
import qualified Data.Tuple as Tuple
import Effectful (Eff, (:>))
import Effectful.Crypto.RNG
import System.Random (RandomGen)
import qualified System.Random as R
import System.Random.Shuffle

shuffleRNG :: (RNG :> es) => [a] -> Eff es [a]
shuffleRNG elements
  | null elements = return []
  | otherwise = do
      fmap (shuffle elements) (rseqM (length elements - 1))
  where
    rseqM :: (RNG :> es) => Int -> Eff es [Int]
    rseqM n = traverse (\i -> randomR (0, i)) [n, n-1 .. 1]

-- | Uniform shuffle. Draws r_i from [0, n-i] for i = 1..n-1, the sample
-- sequence 'System.Random.Shuffle.shuffle' expects.
shuffleList :: (RandomGen g) => [a] -> g -> ([a], g)
shuffleList [] g = ([], g)
shuffleList xs g =
  let n = length xs
      (g', samples) = mapAccumL (\gen i -> Tuple.swap (R.randomR (0, i) gen)) g [n - 1, n - 2 .. 1]
   in (shuffle xs samples, g')
