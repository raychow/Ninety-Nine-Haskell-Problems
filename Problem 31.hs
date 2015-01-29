-- 2 Problem 31
-- (**) Determine whether a given integer number is prime.

-- Example:

-- * (is-prime 7)
-- T

-- Example in Haskell:

-- P31> isPrime 7
-- True

isPrime :: Int -> Bool
isPrime x = all ((/=) 0 . mod x) [2..floor . sqrt $ fromIntegral x]
