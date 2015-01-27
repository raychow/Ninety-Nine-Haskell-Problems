-- 7 Problem 17
-- (*) Split a list into two parts; the length of the first part is given.

-- Do not use any predefined predicates.

-- Example:

-- * (split '(a b c d e f g h i k) 3)
-- ( (A B C) (D E F G H I K))

-- Example in Haskell:

-- *Main> split "abcdefghik" 3
-- ("abc", "defghik")

split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split (x:xs) n
    | n > 0 = (x : a, b)
    | otherwise = (a, x : b)
    where (a, b) = split xs $ n - 1
