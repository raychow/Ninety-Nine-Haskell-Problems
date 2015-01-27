-- 3 Problem 23
-- Extract a given number of randomly selected elements from a list.

-- Example:

-- * (rnd-select '(a b c d e f g h) 3)
-- (E D A)

-- Example in Haskell:

-- Prelude System.Random> rnd_select "abcdefgh" 3 >>= putStrLn
-- eda

import qualified Control.Monad as M
import qualified Data.List as L
import qualified System.Random as R

rnd_select :: (Eq a) => [a] -> Int -> IO [a]
rnd_select [] _ = return []
rnd_select xs n
    | n < 0 = error "invalid number."
    | otherwise = do
        gen <- R.getStdGen
        return $ take n . L.nub $ [xs!!p | p <- R.randomRs (0, length xs - 1) gen]
