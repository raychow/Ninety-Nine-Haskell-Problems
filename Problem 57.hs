-- 5 Problem 57
-- (**) Binary search trees (dictionaries)

-- Use the predicate add/3, developed in chapter 4 of the course,
-- to write a predicate to construct a binary search tree from a list of integer numbers.

-- Example:

-- * construct([3,2,5,7,1],T).
-- T = t(3, t(2, t(1, nil, nil), nil), t(5, nil, t(7, nil, nil)))
-- Then use this predicate to test the solution of the problem P56.

-- Example:

-- * test-symmetric([5,3,18,1,4,12,21]).
-- Yes
-- * test-symmetric([3,2,5,7,4]).
-- No

-- Example in Haskell:

-- *Main> construct [3, 2, 5, 7, 1]
-- Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))
-- *Main> symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
-- True
-- *Main> symmetric . construct $ [3, 2, 5, 7, 1]
-- True

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

construct :: Ord a => [a] -> Tree a
construct = foldl add Empty
    where
        add :: Ord a => Tree a -> a -> Tree a
        add Empty x = Branch x Empty Empty
        add (Branch n l r) x
            | x <= n = Branch n (add l x) r
            | otherwise = Branch n l (add r x)
