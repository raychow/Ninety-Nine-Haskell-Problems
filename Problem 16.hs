-- 6 Problem 16
-- (**) Drop every N'th element from a list.

-- Example:

-- * (drop '(a b c d e f g h i k) 3)
-- (A B D E G H K)

-- Example in Haskell:

-- *Main> dropEvery "abcdefghik" 3
-- "abdeghk"

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = reverse . snd $ foldl helper (1, []) xs
    where
        helper :: (Int, [a]) -> a -> (Int, [a])
        helper (i, a) b
            | i == n = (1, a)
            | otherwise = (i + 1, b : a)
