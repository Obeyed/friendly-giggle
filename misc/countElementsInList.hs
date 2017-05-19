{-# OPTIONS_GHC -Wall #-}

-- Write a function that computes the number of elements in a list.
-- To test it, ensure that it gives the same answers as the standard length function.
countElementsInList :: [a] -> Int
countElementsInList (_:xs) = 1 + countElementsInList xs
countElementsInList [] = 0

