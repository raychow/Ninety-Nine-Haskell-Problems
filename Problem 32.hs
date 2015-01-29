-- 3 Problem 32
-- (**) Determine the greatest common divisor of two positive integer numbers.
-- Use Euclid's algorithm.

-- Example:

-- * (gcd 36 63)
-- 9

-- Example in Haskell:

-- [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
-- [9,3,3]

myGCD :: Integer -> Integer -> Integer
myGCD a 0 = abs a
myGCD a b = myGCD b $ a `mod` b
