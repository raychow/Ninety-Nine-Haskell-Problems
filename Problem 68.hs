-- 11 Problem 68
-- Preorder and inorder sequences of binary trees.
-- We consider binary trees with nodes that are identified by single lower-case letters, as in the example of problem P67.

-- a) Write predicates preorder/2 and inorder/2 that construct the preorder and inorder sequence of a given binary tree, respectively.
-- The results should be atoms, e.g. 'abdecfg' for the preorder sequence of the example in problem P67.

-- b) Can you use preorder/2 from problem part a) in the reverse direction;
-- i.e. given a preorder sequence, construct a corresponding tree?
-- If not, make the necessary arrangements.

-- c) If both the preorder sequence and the inorder sequence of the nodes of a binary tree are given, then the tree is determined unambiguously.
-- Write a predicate pre_in_tree/3 that does the job.

-- Example in Haskell:

-- Main> let { Just t = stringToTree "a(b(d,e),c(,f(g,)))" ;
--             po = treeToPreorder t ;
--             io = treeToInorder t } in preInTree po io >>= print
-- Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty))

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

treeToPreorder :: Tree Char -> String
treeToPreorder Empty = []
treeToPreorder (Branch d l r) = d : treeToPreorder l ++ treeToPreorder r

treeToInorder :: Tree Char -> String
treeToInorder Empty = []
treeToInorder (Branch d l r) = treeToInorder l ++ (d : treeToInorder r)

preInTree :: String -> String -> Tree Char
preInTree [] [] = Empty
preInTree (x:xs) ys = let (yl, _:yr) = break (==x) ys
                          (xl, xr) = splitAt (length yl) xs
                       in Branch x (preInTree xl yl) (preInTree xr yr)
