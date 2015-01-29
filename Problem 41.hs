-- 12 Problem 41
-- (**) Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.

-- In most cases, if an even number is written as the sum of two prime numbers, one of them is very small.
-- Very rarely, the primes are both bigger than say 50.
-- Try to find out how many such cases there are in the range 2..3000.

-- Example:

-- * (goldbach-list 9 20)
-- 10 = 3 + 7
-- 12 = 5 + 7
-- 14 = 3 + 11
-- 16 = 3 + 13
-- 18 = 5 + 13
-- 20 = 3 + 17
-- * (goldbach-list 1 2000 50)
-- 992 = 73 + 919
-- 1382 = 61 + 1321
-- 1856 = 67 + 1789
-- 1928 = 61 + 1867

-- Example in Haskell:

-- *Exercises> goldbachList 9 20
-- [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
-- *Exercises> goldbachList' 4 2000 50
-- [(73,919),(61,1321),(67,1789),(61,1867)]

isPrime :: Integer -> Bool
isPrime x = all ((/=) 0 . mod x) [2..floor . sqrt $ fromIntegral x]

primeR :: Integer -> Integer -> [Integer]
primeR a b = [x | x <- [a..b], isPrime x]

goldbach :: Integer -> (Integer, Integer)
goldbach n = head [(x, y) | x <- primes, y <- primes, x + y == n]
    where primes = primeR 2 $ n - 2

goldbachList :: Integer -> Integer -> [(Integer, Integer)]
goldbachList a b = [goldbach x | x <- [a..b], even x]

goldbachList' :: Integer -> Integer -> Integer -> [(Integer, Integer)]
goldbachList' a b n = filter (\(x, y) -> x > n && b > n) $ goldbachList a b
