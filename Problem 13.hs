-- 3 Problem 13
-- (**) Run-length encoding of a list (direct solution).

-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them.
-- As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

-- Example:

-- * (encode-direct '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))

-- Example in Haskell:

-- P13> encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']

data Element a = Single a | Multiple Int a deriving (Show)

encodeDirect :: (Eq a) => [a] -> [Element a]
encodeDirect = foldr helper []
    where
        helper :: (Eq a) => a -> [Element a] -> [Element a]
        helper x [] = [Single x]
        helper x yss@(Single y:ys)
            | x == y = Multiple 2 x : ys
            | otherwise = Single x : yss
        helper x yss@(Multiple n y:ys)
            | x == y = Multiple (n + 1) x : ys
            | otherwise = Single x : yss
