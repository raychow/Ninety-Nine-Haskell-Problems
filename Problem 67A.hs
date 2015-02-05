-- 10 Problem 67A
-- A string representation of binary trees

-- Somebody represents binary trees as strings of the following type:

-- a(b(d,e),c(,f(g,)))
-- a) Write a Prolog predicate which generates this string representation, if the tree is given as usual (as nil or t(X,L,R) term).
-- Then write a predicate which does this inverse; i.e. given the string representation, construct the tree in the usual form.
-- Finally, combine the two predicates in a single predicate tree_string/2 which can be used in both directions.

-- Example in Prolog

-- ?- tree_to_string(t(x,t(y,nil,nil),t(a,nil,t(b,nil,nil))),S).
-- S = 'x(y,a(,b))'
-- ?- string_to_tree('x(y,a(,b))',T).
-- T = t(x, t(y, nil, nil), t(a, nil, t(b, nil, nil)))

-- Example in Haskell:

-- Main> stringToTree "x(y,a(,b))" >>= print
-- Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))
-- Main> let t = cbtFromList ['a'..'z'] in stringToTree (treeToString t) >>= print . (== t)
-- True

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

stringToTree :: String -> Maybe (Tree Char)
stringToTree = helper
    where
        helper :: String -> Maybe (Tree Char)
        helper xs = do
            (t, xs') <- branchConverter xs
            if null xs' then return t else Nothing

        branchConverter :: String -> Maybe (Tree Char, String)
        branchConverter [] = Just (Empty, "")
        branchConverter (d:'(':xs) = do
            (l, xs') <- leafConverter ',' xs
            (r, xs'') <- leafConverter ')' xs'
            return (Branch d l r, xs'')
        branchConverter (d:xs) = Just (Branch d Empty Empty, xs)

        leafConverter :: Char -> String -> Maybe (Tree Char, String)
        leafConverter _ [] = Nothing
        leafConverter sep xxs@(x:xs)
            | sep == x = Just (Empty, xs)
            | otherwise = do
                (t, s:xxs') <- branchConverter xxs
                if s == sep then return (t, xxs') else Nothing

treeToString :: Tree Char -> String
treeToString Empty = ""
treeToString (Branch d Empty Empty) = [d]
treeToString (Branch d l r) = d : '(' : treeToString l ++ (',' : treeToString r) ++ ")"
