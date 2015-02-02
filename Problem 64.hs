-- 7 Problem 64
-- Given a binary tree as the usual Prolog term t(X,L,R) (or nil).
-- As a preparation for drawing the tree, a layout algorithm is required to determine the position of each node in a rectangular grid.
-- Several layout methods are conceivable, one of them is shown in the illustration below:

-- p64.gif

-- In this layout strategy, the position of a node v is obtained by the following two rules:

-- x(v) is equal to the position of the node v in the inorder sequence
-- y(v) is equal to the depth of the node v in the tree
-- Write a function to annotate each node of the tree with a position,
-- where (1,1) in the top left corner or the rectangle bounding the drawn tree.

-- Here is the example tree from the above illustration:

-- tree64 = Branch 'n'
--                 (Branch 'k'
--                         (Branch 'c'
--                                 (Branch 'a' Empty Empty)
--                                 (Branch 'h'
--                                         (Branch 'g'
--                                                 (Branch 'e' Empty Empty)
--                                                 Empty
--                                         )
--                                         Empty
--                                 )
--                         )
--                         (Branch 'm' Empty Empty)
--                 )
--                 (Branch 'u'
--                         (Branch 'p'
--                                 Empty
--                                 (Branch 's'
--                                         (Branch 'q' Empty Empty)
--                                         Empty
--                                 )
--                         )
--                         Empty
--                 )

-- Example in Haskell:

-- > layout tree64
-- Branch ('n',(8,1)) (Branch ('k',(6,2)) (Branch ('c',(2,3)) ...

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

tree64 = Branch 'n'
        (Branch 'k'
            (Branch 'c'
                (Branch 'a' Empty Empty)
                (Branch 'h'
                    (Branch 'g'
                        (Branch 'e' Empty Empty)
                        Empty
                    )
                    Empty
                )
            )
            (Branch 'm' Empty Empty)
        )
        (Branch 'u'
            (Branch 'p'
                Empty
                (Branch 's'
                    (Branch 'q' Empty Empty)
                    Empty
                )
            )
            Empty
        )

layout :: Tree a -> Tree (a, (Integer, Integer))
layout t = fst $ helper t 1 1
    where
        helper :: Tree a -> Integer -> Integer -> (Tree (a, (Integer, Integer)), Integer)
        helper Empty n _          = (Empty, n)
        helper (Branch x l r) n h = let (l', nl) = helper l n (h + 1)
                                        (r', nr) = helper r (nl + 1) (h + 1)
                                     in (Branch (x, (nl, h)) l' r', nr)
