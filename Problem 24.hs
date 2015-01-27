-- 4 Problem 24
-- Lotto: Draw N different random numbers from the set 1..M.

-- Example:

-- * (rnd-select 6 49)
-- (23 1 17 33 21 37)

-- Example in Haskell:

-- Prelude System.Random> diff_select 6 49
-- Prelude System.Random> [23,1,17,33,21,37]

import Control.Applicative
import qualified Data.List as L
import qualified System.Random as R

diff_select :: Int -> Int -> IO [Int]
diff_select n m = take n . L.nub . R.randomRs (1, m) <$> R.getStdGen
