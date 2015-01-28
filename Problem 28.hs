-- 8 Problem 28
-- Sorting a list of lists according to length of sublists

-- a) We suppose that a list contains elements that are lists themselves.
-- The objective is to sort the elements of this list according to their length.
-- E.g. short lists first, longer lists later, or vice versa.

-- Example:

-- * (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
-- ((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))

-- Example in Haskell:

-- Prelude> lsort ["abc","de","fgh","de","ijkl","mn","o"]
-- Prelude> ["o","de","de","mn","abc","fgh","ijkl"]

-- b) Again, we suppose that a list contains elements that are lists themselves.
-- But this time the objective is to sort the elements of this list according to their length frequency;
-- i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.

-- Example:

-- * (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
-- ((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n))

-- Example in Haskell:

-- lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
-- ["ijkl","o","abc","fgh","de","de","mn"]

import           Data.Function (on)
import qualified Data.List as L
import           Data.Map ((!))
import qualified Data.Map as M

lsort :: [[a]] -> [[a]]
lsort = L.sortBy (compare `on` length)

lfsort :: [[a]] -> [[a]]
lfsort xs = L.sortBy helper xs
    where
        helper :: [a] -> [a] -> Ordering
        helper xs ys = compare (f!length xs) (f!length ys)

        f :: M.Map Int Int
        f = M.fromListWith (+) [(length x, 1) | x <- xs]
