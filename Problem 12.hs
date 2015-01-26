-- 2 Problem 12
-- (**) Decode a run-length encoded list.

-- Given a run-length code list generated as specified in problem 11.
-- Construct its uncompressed version.

-- Example in Haskell:

-- P12> decodeModified
--        [Multiple 4 'a',Single 'b',Multiple 2 'c',
--         Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"

data Element a = Single a | Multiple Int a deriving (Show)

decodeModified :: [Element a] -> [a]
decodeModified = concatMap convert
    where
        convert :: Element a -> [a]
        convert (Single x) = [x]
        convert (Multiple n x) = replicate n x
