-- 6 Problem 58
-- (**) Generate-and-test paradigm

-- Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.

-- Example:

-- * sym-cbal-trees(5,Ts).
-- Ts = [t(x, t(x, nil, t(x, nil, nil)), t(x, t(x, nil, nil), nil)), t(x, t(x, t(x, nil, nil), nil), t(x, nil, t(x, nil, nil)))]

-- Example in Haskell:

-- *Main> symCbalTrees 5
-- [Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty),Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))]

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

cbalTree :: Integer -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n
    | even n = do
        left <- cbalTree (n `div` 2)
        right <- cbalTree (n `div` 2 - 1)
        [Branch 'x' left right, Branch 'x' right left]
    | otherwise = [Branch 'x' left right | left <- cbalTree (n `div` 2),
                                           right <- cbalTree (n `div` 2)]

symCbalTrees :: Integer -> [Tree Char]
symCbalTrees n
    | even n = error "must be odd."
    | otherwise = [Branch 'x' t (reverseTree t) | t <- cbalTree $ n `div` 2]
    where
        reverseTree :: Tree Char -> Tree Char
        reverseTree Empty = Empty
        reverseTree (Branch n l r) = Branch n (reverseTree r) (reverseTree l)
