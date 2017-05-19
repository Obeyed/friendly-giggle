{-# OPTIONS_GHC -Wall #-}

-- Turn a list into a palindrome, i.e. it should read the same both
-- backwards and forwards. For example, given the list [1,2,3],
-- your function should return [1,2,3,3,2,1].
toPalindrome :: [a] -> [a]
toPalindrome [] = []
toPalindrome (x:xs) = [x] ++ toPalindrome xs ++ [x]


-- Write a function that determines whether its input list is a palindrome.
palindrome :: Eq a => [a] -> Bool
palindrome [] = True
palindrome xs = (head(xs) == last(xs)) && (palindrome (tail (init xs)))

palindrome' :: Eq a => [a] -> Bool
palindrome' xs = xs == reverse xs
