{-# OPTIONS_GHC -Wall #-}

module Curry where

{- Fourth homework http://www.seas.upenn.edu/~cis194/spring13/hw/04-higher-order.pdf -}

-- Exercise 1
{-
 - Reimplement each of the following functions in a more idiomatic
 - Haskell style. Use wholemeal programming practices, breaking each
 - function into a pipeline of incremental transformations to an entire
 - data structure.
 -}
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

-- fun1 == fun1'
fun1' :: [Integer] -> Integer
fun1' = foldl (\acc x -> (x - 2) * acc) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

-- fun2 == fun2'
fun2' :: Integer -> Integer
fun2' = sum
        . filter even
        . takeWhile (/=1)
        . iterate (\x -> if even x
                         then x `div` 2
                         else x * 3 + 1)

-- fun2'' == fun2'
fun2'' :: Integer -> Integer
fun2'' = sum
         . filter even
         . takeWhile (/=1)
         . iterate (\x -> if even x
                          then (`div` 2) x
                          else (1+) . (*3) $ x )
