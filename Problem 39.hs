-- 10 Problem 39
-- (*) A list of prime numbers.

-- Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.

-- Example in Haskell:

-- P29> primesR 10 20
-- [11,13,17,19]

isPrime :: Integer -> Bool
isPrime x = all ((/=) 0 . mod x) [2..floor . sqrt $ fromIntegral x]

primeR :: Integer -> Integer -> [Integer]
primeR a b = [x | x <- [a..b], isPrime x]
