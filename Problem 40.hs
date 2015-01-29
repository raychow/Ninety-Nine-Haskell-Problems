-- 11 Problem 40
-- (**) Goldbach's conjecture.

-- Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers.
-- Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case.
-- It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system).
-- Write a predicate to find the two prime numbers that sum up to a given even integer.

-- Example:

-- * (goldbach 28)
-- (5 23)

-- Example in Haskell:

-- *goldbach 28
-- (5, 23)

isPrime :: Integer -> Bool
isPrime x = all ((/=) 0 . mod x) [2..floor . sqrt $ fromIntegral x]

primeR :: Integer -> Integer -> [Integer]
primeR a b = [x | x <- [a..b], isPrime x]

goldbach :: Integer -> (Integer, Integer)
goldbach n = head [(x, y) | x <- primes, y <- primes, x + y == n]
    where primes = primeR 2 $ n - 2
