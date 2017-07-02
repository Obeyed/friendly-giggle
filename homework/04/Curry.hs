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


-- Exercise 2
{- Recall the definition of a binary tree data structure. The
 - height of a binary tree is the length of a path from the root to the deepest
 - node. For example, the height of a tree with a single node is 0; the
 - height of a tree with three nodes, whose root has two children, is 1;
 - and so on. A binary tree is balanced if the height of its left and right
 - subtrees differ by no more than 1, and its left and right subtrees are
 - also balanced.
 -}
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf

-- https://stackoverflow.com/a/35957393/3169491
insertNode :: a -> Tree a -> Tree a
insertNode n Leaf = Node 0 Leaf n Leaf
insertNode n (Node depth leftTree elm rightTree)
  | leftHeight < rightHeight       = Node depth newLeftTree elm rightTree
  | leftHeight > rightHeight       = Node depth leftTree elm newRightTree
  | newLeftHeight < newRightHeight = Node depth newLeftTree elm rightTree
  | otherwise                      = Node (newRightHeight + 1) leftTree elm newRightTree
  where leftHeight     = treeHeight leftTree
        rightHeight    = treeHeight rightTree
        newLeftTree    = insertNode n leftTree
        newRightTree   = insertNode n rightTree
        newLeftHeight  = treeHeight newLeftTree
        newRightHeight = treeHeight newRightTree

treeHeight :: Tree a -> Integer
treeHeight Leaf               = 0
treeHeight (Node depth _ _ _) = depth

-- https://stackoverflow.com/a/16165334/3169491
-- useful sideways visualisation
showTree :: Show a => Tree a -> [Char]
showTree Leaf                 = ""
showTree n@(Node depth _ _ _) = go depth n
  where go _ (Leaf) = ""
        go i (Node _ l c r) = go (i-1) l ++
            replicate (4*fromIntegral i) ' ' ++ show c ++ "\n" ++ go (i-1) r

-- Exercise 3a
{- Implement a function
 -   xor :: [Bool] -> Bool
 - which returns True if and only if there are an odd number of True
 - values contained in the input list. It does not matter how many
 - False values the input list contains. For example,
 -   xor [False, True, False] == True
 -   xor [False, True, False, False, True] == False
 - Your solution must be implemented using a fold.
 -}
xor :: [Bool] -> Bool
xor = foldl (\x acc -> if x then not acc else acc) False

-- Exercise 3b
{- Implement map as a fold. That is, complete the definition
 -   map’ :: (a -> b) -> [a] -> [b]
 -   map’ f = foldr ...
 - in such a way that map’ behaves identically to the
 - standard map function.
 -}
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- Exercise 3c
{- (Optional) Implement foldl using foldr. That is,
 - complete the definition
 -   myFoldl :: (a -> b -> a) -> a -> [b] -> a
 -   myFoldl f base xs = foldr ...
 - in such a way that myFoldl behaves identically to the standard
 - foldl function.
 - Hint: Study how the application of foldr and foldl work out:
 -   foldr f z [x1, x2, ..., xn] == x1 ‘f‘ (x2 ‘f‘ ... (xn ‘f‘ z)...)
 -   foldl f z [x1, x2, ..., xn] == (...((z ‘f‘ x1) ‘f‘ x2) ‘f‘...) ‘f‘ xn
 -}
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr ...
