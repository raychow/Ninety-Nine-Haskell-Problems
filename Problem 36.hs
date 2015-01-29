-- 7 Problem 36
-- (**) Determine the prime factors of a given positive integer.

-- Construct a list containing the prime factors and their multiplicity.

-- Example:

-- * (prime-factors-mult 315)
-- ((3 2) (5 1) (7 1))

-- Example in Haskell:

-- *Main> prime_factors_mult 315
-- [(3,2),(5,1),(7,1)]

import Data.List (group)

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = let prime' = dropWhile ((/=) 0 . mod n) [2..floor . sqrt $ fromInteger n]
                     prime = if null prime' then n else head prime'
                  in prime : primeFactors (n `div` prime)

prime_factors_mult :: Integer -> [(Integer, Integer)]
prime_factors_mult = map stat . group . primeFactors
    where
        stat :: [Integer] -> (Integer, Integer)
        stat xs = (head xs, toInteger $ length xs)
