-- 3 Problem 47
-- (*) Truth tables for logical expressions (2).

-- Continue problem P46 by defining and/2, or/2, etc as being operators.
-- This allows to write the logical expression in the more natural way, as in the example: A and (A or not B).
-- Define operator precedence as usual; i.e. as in Java.

-- Example:

-- * (table A B (A and (A or not B)))
-- true true true
-- true fail true
-- fail true fail
-- fail fail fail

-- Example in Haskell:

-- > table2 (\a b -> a `and'` (a `or'` not b))
-- True True True
-- True False True
-- False True False
-- False False False

not' :: Bool -> Bool
not' True = False
not' False = True
infixr 6 `not'`

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False
infixr 4 `and'`

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True
infixr 2 `or'`

nand' :: Bool -> Bool -> Bool
nand' a b = not' $ and' a b
infixr 4 `nand'`

nor' :: Bool -> Bool -> Bool
nor' a b = not' $ or' a b
infixr 2 `nor'`

xor' :: Bool -> Bool -> Bool
xor' a b
    | a `equ'` b = True
    | otherwise = False
infixr 3 `xor'`

impl' :: Bool -> Bool -> Bool
impl' a b = not' a `or'` b
infixr 4 `impl'`

equ' :: Bool -> Bool -> Bool
equ' True True = True
equ' False False = True
equ' _ _ = False
infixr 5 `equ'`

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ (\(a, b) -> putStrLn $ show a ++ " " ++ show b ++ " " ++ show (f a b))
                [(x, y) | x <- [True, False], y <- [True, False]]
