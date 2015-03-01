-- 8 Problem 65
-- An alternative layout method is depicted in the illustration below:

-- p65.gif

-- Find out the rules and write the corresponding function. Hint: On a given level, the horizontal distance between neighboring nodes is constant.

-- Use the same conventions as in problem P64 and test your function in an appropriate way.

-- Here is the example tree from the above illustration:

-- tree65 = Branch 'n'
--                 (Branch 'k'
--                         (Branch 'c'
--                                 (Branch 'a' Empty Empty)
--                                 (Branch 'e'
--                                         (Branch 'd' Empty Empty)
--                                         (Branch 'g' Empty Empty)
--                                 )
--                         )
--                         (Branch 'm' Empty Empty)
--                 )
--                 (Branch 'u'
--                         (Branch 'p'
--                                 Empty
--                                 (Branch 'q' Empty Empty)
--                         )
--                         Empty
--                 )

-- Example in Haskell:

-- > layout tree65
-- Branch ('n',(15,1)) (Branch ('k',(7,2)) (Branch ('c',(3,3)) ...

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
layout t = layout' t (2 ^ (h - 1) - 1) 1
    where
        h = height t

        height :: Tree a -> Integer
        height Empty = 0
        height (Branch _ l r) = max (height l) (height r) + 1

        layout' :: Tree a -> Integer -> Integer -> Tree (a, (Integer, Integer))
        layout' Empty _ _ = Empty
        layout' (Branch d l r) x h' = Branch (d, (x, h')) l' r'
            where
                l' = layout' l (x - w) (h' + 1)
                r' = layout' r (x + w) (h' + 1)
                w  = 2 ^ (h - h' - 1)
