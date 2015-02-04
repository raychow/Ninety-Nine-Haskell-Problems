-- 9 Problem 66
-- Yet another layout strategy is shown in the illustration below:

-- p66.gif

-- The method yields a very compact layout while maintaining a certain symmetry in every node.
-- Find out the rules and write the corresponding Prolog predicate.
-- Hint: Consider the horizontal distance between a node and its successor nodes.
-- How tight can you pack together two subtrees to construct the combined binary tree?

-- Use the same conventions as in problem P64 and P65 and test your predicate in an appropriate way.
-- Note: This is a difficult problem. Don't give up too early!

-- Which layout do you like most?

-- Example in Haskell:

-- > layout tree65
-- Branch ('n',(5,1)) (Branch ('k',(3,2)) (Branch ('c',(2,3)) ...

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

tree65 = Branch 'n'
        (Branch 'k'
            (Branch 'c'
                (Branch 'a' Empty Empty)
                (Branch 'e'
                    (Branch 'd' Empty Empty)
                    (Branch 'g' Empty Empty)
                )
            )
            (Branch 'm' Empty Empty)
        )
        (Branch 'u'
            (Branch 'p'
                Empty
                (Branch 'q' Empty Empty)
            )
            Empty
        )

layout :: Tree a -> Tree (a, (Integer, Integer))
layout t = root
    where
        rootX = maximum rootLD + 1
        (rootLD, root, _) = helper t rootX 1

        helper :: Tree a -> Integer -> Integer -> ([Integer], Tree (a, (Integer, Integer)), [Integer])
        helper Empty x y = ([], Empty, [])
        helper (Branch d l r) x y = (lld', Branch (d, (x, y)) l' r', rrd')
            where
                (lld, l', lrd) = helper l (x - sep) (y + 1)
                (rld, r', rrd) = helper r (x + sep) (y + 1)
                sep = maximum (0 : zipWith (+) lrd rld) `div` 2 + 1
                lld' = 0 : leftOverlap (map (+sep) lld) (map (subtract sep) rld)
                rrd' = 0 : leftOverlap (map (+sep) rrd) (map (subtract sep) lrd)

                leftOverlap :: [a] -> [a] -> [a]
                leftOverlap xs [] = xs
                leftOverlap [] ys = ys
                leftOverlap (x:xs) (y:ys) = x : leftOverlap xs ys
