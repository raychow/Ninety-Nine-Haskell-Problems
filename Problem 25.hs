-- 5 Problem 25
-- Generate a random permutation of the elements of a list.

-- Example:

-- * (rnd-permu '(a b c d e f))
-- (B A D C E F)

-- Example in Haskell:

-- Prelude System.Random> rnd_permu "abcdef"
-- Prelude System.Random> "badcef"

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

rnd_permu :: (Eq a) => [a] -> IO [a]
rnd_permu xs = rnd_select xs $ length xs
