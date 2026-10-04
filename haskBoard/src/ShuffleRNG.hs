module ShuffleRNG (shuffleList) where

import Data.List (mapAccumL)
import qualified Data.Tuple as Tuple
import System.Random (RandomGen)
import qualified System.Random as R
import System.Random.Shuffle

-- | Uniform shuffle. Draws r_i from [0, n-i] for i = 1..n-1, the sample
-- sequence 'System.Random.Shuffle.shuffle' expects.
shuffleList :: (RandomGen g) => [a] -> g -> ([a], g)
shuffleList [] g = ([], g)
shuffleList xs g =
  let n = length xs
      (g', samples) = mapAccumL (\gen i -> Tuple.swap (R.randomR (0, i) gen)) g [n - 1, n - 2 .. 1]
   in (shuffle xs samples, g')
