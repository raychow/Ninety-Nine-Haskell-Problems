-- 7 Problem 59
-- (**) Construct height-balanced binary trees

-- In a height-balanced binary tree, the following property holds for every node:
-- The height of its left subtree and the height of its right subtree are almost equal,
-- which means their difference is not greater than one.

-- Construct a list of all height-balanced binary trees with the given element and the given maximum height.

-- Example:

-- ?- hbal_tree(3,T).
-- T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), t(x, nil, nil))) ;
-- T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), nil)) ;
-- etc......No

-- Example in Haskell:

-- *Main> take 4 $ hbalTree 'x' 3
-- [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)),
--  Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),
--  Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)),
--  Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)]

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

hbalTree :: a -> Integer -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree n 1 = [Branch n Empty Empty]
hbalTree n h = do
    (lh, rh) <- [(h - 1, h - 1),
                 (h - 2, h - 1),
                 (h - 1, h - 2)]
    left <- hbalTree n lh
    right <- hbalTree n rh
    return $ Branch n left right
