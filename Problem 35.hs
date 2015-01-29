-- 6 Problem 35
-- (**) Determine the prime factors of a given positive integer.
--  Construct a flat list containing the prime factors in ascending order.

-- Example:

-- * (prime-factors 315)
-- (3 3 5 7)

-- Example in Haskell:

-- > primeFactors 315
-- [3, 3, 5, 7]

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = let prime' = dropWhile ((/=) 0 . mod n) [2..floor . sqrt $ fromInteger n]
                     prime = if null prime' then n else head prime'
                  in prime : primeFactors (n `div` prime)
