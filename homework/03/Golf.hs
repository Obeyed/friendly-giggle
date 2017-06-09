{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.List (zip3, intersperse, transpose)

{- Third homework http://www.seas.upenn.edu/~cis194/spring13/hw/03-rec-poly.pdf -}

-- Exercise 1
{-
 - The output of skips is a list of lists. The first list in
 - the output should be the same as the input list.
 - The second list in the output should contain every
 - second element from the input list... and the nth list
 - in the output should contain every nth element from the input list.
 -}
skips :: [a] -> [[a]]
skips xs = reverse $ skipsIterator (length (xs)) (xs)

skipsIterator :: Int -> [a] -> [[a]]
skipsIterator 0 _  = []
skipsIterator n xs = [(skipsJump n xs)] ++ (skipsIterator (n-1) xs)

-- https://stackoverflow.com/a/2028218/3169491
skipsJump :: Int -> [a] -> [a]
skipsJump n xs = case drop (n-1) xs of
                 (y:ys) -> y : skipsJump n ys
                 [] -> []


-- Exercise 2
{-
 - A local maximum of a list is an element of the list which is strictly
 - greater than both the elements immediately before and after it. For
 - example, in the list [2,3,4,1,5], the only local maximum is 4,
 - since it is greater than the elements immediately before and after it
 - (3 and 1). 5 is not a local maximum since there is no element that
 - comes after it.
 -}
-- Third attempt (using foldr instead of map and filter)
localMaxima :: [Integer] -> [Integer]
localMaxima (x:x':xs) = foldr
                        (\y ys -> if isLocalMax y
                                  then (getMiddle y):ys
                                  else ys)
                        []
                        $ zip3 (x:x':xs) (x':xs) xs
  where isLocalMax (a,b,c) = b > a && b > c
        getMiddle (_,m,_) = m
localMaxima _ = []

-- Second attempt (using zip3, map, and filter)
-- inspiration from https://stackoverflow.com/a/22047409/3169491
-- 1. create triples of the neighbouring values
-- 2. filter out triples that do no have local max as middle element
-- 3. get the middle elements
localMaxima' :: [Integer] -> [Integer]
localMaxima' (x:x':xs) = map getMiddle'
                         . filter isLocalMax'
                         $ zip3 (x:x':xs) (x':xs) xs
  where isLocalMax' (a,b,c) = b > a && b > c
        getMiddle' (_,m,_) = m
localMaxima' _ = []

-- First attempt
localMaxima'' :: [Integer] -> [Integer]
localMaxima'' (a:b:c:xs) = getLocalMax'' a b c ++ localMaxima'' (b:c:xs)
localMaxima'' _          = []

getLocalMax'' :: Integer -> Integer -> Integer -> [Integer]
getLocalMax'' a b c = if (b > a && b > c) then [b] else []


-- Exercise 3
{-
 - takes as input a list of Integers between 0 and 9 (inclusive),
 - and outputs a vertical histogram showing how many of each number
 - were in the input list. You may assume that the input list does not
 - contain any numbers less than zero or greater than 9 (that is,
 - it does not matter what your function does if the input does
 - contain such numbers).
 -}
-- test it in GHCi with
--   putStr $ histogram [1,4,5,4,6,6,3,4,2,4,9]
histogram :: [Integer] -> String
histogram xs = (concat $ intersperse "\n" $ transpose listifiedGraph)
               ++ footer
  where
    footer = "\n" ++ (replicate 10 '=') ++ "\n" ++ ['0'..'9'] ++ "\n"
    -- count elements for each bucket
    bucketized = bucketize [0..9] xs
    -- create list of strings with num * as size of bucket
    -- and fix list of strings by appending with spaces,
    -- s.t. all strings have same length
    listifedGraph = [ replicate (maximum bucketized - (length x)) ' ' ++ x
                      | x <- [replicate x '*' | x <- bucketized] ]

-- create buckets and count number of occurence of elements
-- that match integer of parameter
bucketize :: [Integer] -> [Integer] -> [Int]
bucketize [] _      = []
bucketize (n:ns) xs = [countBy n xs] ++ bucketize ns xs

-- count occurence of a value
countBy :: Integer -> [Integer] -> Int
countBy n xs = length $ filter (==n) xs
