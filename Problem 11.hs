-- 1 Problem 11
-- (*) Modified run-length encoding.

-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list.
-- Only elements with duplicates are transferred as (N E) lists.

-- Example:

-- * (encode-modified '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))

-- Example in Haskell:

-- P11> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']

import qualified Data.List as L

data Element a = Single a | Multiple Int a deriving (Show)

encodeModified :: (Eq a) => [a] -> [Element a]
encodeModified xs = map (\(y:ys) -> if null ys
    then Single y
    else Multiple (length ys + 1) y) $ L.group xs
