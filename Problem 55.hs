-- 3 Problem 55
-- (**) Construct completely balanced binary trees

-- In a completely balanced binary tree, the following property holds for every node:
-- The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal,
-- which means their difference is not greater than one.

-- Write a function cbal-tree to construct completely balanced binary trees for a given number of nodes.
-- The predicate should generate all solutions via backtracking.
-- Put the letter 'x' as information into all nodes of the tree.

-- Example:

-- * cbal-tree(4,T).
-- T = t(x, t(x, nil, nil), t(x, nil, t(x, nil, nil))) ;
-- T = t(x, t(x, nil, nil), t(x, t(x, nil, nil), nil)) ;
-- etc......No

-- Example in Haskell, whitespace and "comment diagrams" added for clarity and exposition:

-- *Main> cbalTree 4
-- [
-- -- permutation 1
-- --     x
-- --    / \
-- --   x   x
-- --        \
-- --         x
-- Branch 'x' (Branch 'x' Empty Empty)
--            (Branch 'x' Empty
--                        (Branch 'x' Empty Empty)),

-- -- permutation 2
-- --     x
-- --    / \
-- --   x   x
-- --      /
-- --     x
-- Branch 'x' (Branch 'x' Empty Empty)
--            (Branch 'x' (Branch 'x' Empty Empty)
--                        Empty),

-- -- permutation 3
-- --     x
-- --    / \
-- --   x   x
-- --    \
-- --     x
-- Branch 'x' (Branch 'x' Empty
--                        (Branch 'x' Empty Empty))
--            (Branch 'x' Empty Empty),

-- -- permutation 4
-- --     x
-- --    / \
-- --   x   x
-- --  /
-- -- x
-- Branch 'x' (Branch 'x' (Branch 'x' Empty Empty)
--                        Empty)
--            (Branch 'x' Empty Empty)
-- ]

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
