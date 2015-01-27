-- 1 Problem 21
-- Insert an element at a given position into a list.

-- Example:

-- * (insert-at 'alfa '(a b c d) 2)
-- (A ALFA B C D)

-- Example in Haskell:

-- P21> insertAt 'X' "abcd" 2
-- "aXbcd"

import qualified Data.List as L

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = a ++ x : b
    where (a, b) = L.splitAt (n - 1) xs
