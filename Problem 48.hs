-- 4 Problem 48
-- (**) Truth tables for logical expressions (3).

-- Generalize problem P47 in such a way that the logical expression may contain any number of logical variables.
-- Define table/2 in a way that table(List,Expr) prints the truth table for the expression Expr, which contains the logical variables enumerated in List.

-- Example:

-- * (table (A,B,C) (A and (B or C) equ A and B or A and C))
-- true true true true
-- true true fail true
-- true fail true true
-- true fail fail true
-- fail true true true
-- fail true fail true
-- fail fail true true
-- fail fail fail true

-- Example in Haskell:

-- > tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
-- -- infixl 3 `equ'`
-- True  True  True  True
-- True  True  False True
-- True  False True  True
-- True  False False True
-- False True  True  True
-- False True  False True
-- False False True  True
-- False False False True

-- -- infixl 7 `equ'`
-- True  True  True  True
-- True  True  False True
-- True  False True  True
-- True  False False False
-- False True  True  False
-- False True  False False
-- False False True  False
-- False False False False

import Control.Monad (replicateM)

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

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ (\xs -> putStrLn $ unwords (map show xs) ++ " " ++ show (f xs)) $ replicateM n [True, False]
