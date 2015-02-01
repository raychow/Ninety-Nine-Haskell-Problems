-- 8 Problem 60
-- (**) Construct height-balanced binary trees with a given number of nodes

-- Consider a height-balanced binary tree of height H.
-- What is the maximum number of nodes it can contain?

-- Clearly, MaxN = 2**H - 1.
-- However, what is the minimum number MinN? This question is more difficult.
-- Try to find a recursive statement and turn it into a function minNodes that returns the minimum number of nodes in a height-balanced binary tree of height H.
-- On the other hand, we might ask: what is the maximum height H a height-balanced binary tree with N nodes can have?
-- Write a function maxHeight that computes this.
-- Now, we can attack the main problem:
-- construct all the height-balanced binary trees with a given number of nodes.
-- Find out how many height-balanced trees exist for N = 15.

-- Example in Prolog:

-- ?- count_hbal_trees(15,C).
-- C = 1553

-- Example in Haskell:

-- *Main> length $ hbalTreeNodes 'x' 15
-- 1553
-- *Main> map (hbalTreeNodes 'x') [0..3]
-- [[Empty],
--  [Branch 'x' Empty Empty],
--  [Branch 'x' Empty (Branch 'x' Empty Empty),Branch 'x' (Branch 'x' Empty Empty) Empty],
--  [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)]]

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

maxNodes :: Integer -> Integer
maxNodes h = 2 ^ h - 1

minNodes :: Integer -> Integer
minNodes 0 = 0
minNodes 1 = 1
minNodes h = head $ helper h
    where
        helper :: Integer -> [Integer]
        helper 1 = [1, 0]
        helper n = let ys = helper (n - 1)
                       in ys!!0 + ys!!1 + 1 : ys

maxHeight :: Integer -> Integer
maxHeight n = fromIntegral . length . takeWhile (<=n) $ map minNodes [1..]

minHeight :: Integer -> Integer
minHeight n = (floor . logBase 2 $ fromIntegral n) + 1

hbalTreeNodes :: a -> Integer -> [Tree a]
hbalTreeNodes x n = concat [buildTrees x h n | h <- [minHeight n..maxHeight n]]
    where
        buildTrees :: a -> Integer -> Integer -> [Tree a]
        buildTrees x 0 _ = [Empty]
        buildTrees x 1 _ = [Branch x Empty Empty]
        buildTrees x h n = [Branch x ltree rtree | (hl, hr) <- [(h - 1, h - 1),
                                                                (h - 1, h - 2),
                                                                (h - 2, h - 1)],
                                                   nl <- [max (minNodes hl) (n - 1 - maxNodes hr)..min (maxNodes hl) (n - 1 - minNodes hr)],
                                                   let nr = n - 1 - nl,
                                                   ltree <- buildTrees x hl nl,
                                                   rtree <- buildTrees x hr nr]
