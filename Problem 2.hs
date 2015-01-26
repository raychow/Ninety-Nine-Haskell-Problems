-- 2 Problem 2
-- (*) Find the last but one element of a list.

-- (Note that the Lisp transcription of this problem is incorrect.)

-- Example in Haskell:

-- Prelude> myButLast [1,2,3,4]
-- 3
-- Prelude> myButLast ['a'..'z']
-- 'y'

myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast [_] = error "the list only has 1 element."
myButLast (x:[_]) = x
myButLast (x:xs) = myButLast xs
