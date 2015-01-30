-- 2 Problem 46
-- (**) Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations;
-- e.g. and(A,B) will succeed, if and only if both A and B succeed.

-- A logical expression in two variables can then be written as in the following example: and(or(A,B),nand(A,B)).

-- Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.

-- Example:

-- (table A B (and A (or A B)))
-- true true true
-- true fail true
-- fail true fail
-- fail fail fail

-- Example in Haskell:

-- > table (\a b -> (and' a (or' a b)))
-- True True True
-- True False True
-- False True False
-- False False False

not' :: Bool -> Bool
not' True = False
not' False = True

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

nand' :: Bool -> Bool -> Bool
nand' a b = not' $ and' a b

nor' :: Bool -> Bool -> Bool
nor' a b = not' $ or' a b

xor' :: Bool -> Bool -> Bool
xor' a b
    | a `equ'` b = True
    | otherwise = False

impl' :: Bool -> Bool -> Bool
impl' a b = not' a `or'` b

equ' :: Bool -> Bool -> Bool
equ' True True = True
equ' False False = True
equ' _ _ = False

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ (\(a, b) -> putStrLn $ show a ++ " " ++ show b ++ " " ++ show (f a b))
                [(x, y) | x <- [True, False], y <- [True, False]]
