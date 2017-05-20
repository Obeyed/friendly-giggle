{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

{- Second homework http://www.seas.upenn.edu/~cis194/spring13/hw/02-ADTs.pdf -}

-- Exercise 1

-- test this in GHCi with
--    testParse parse 10 "error.log"
parse :: String -> [LogMessage]
parse xs = map parseMessage $ lines xs

-- Third attempt
-- avoid parenthesis by using the (.) and ($) operators
parseMessage :: String -> LogMessage
parseMessage ('I':xs) = LogMessage Info ts msg
  where ts  = read . head $ ws :: Int
        msg = unwords . drop 1 $ ws
        ws  = words xs
parseMessage ('W':xs) = LogMessage Warning ts msg
  where ts  = read . head $ ws :: Int
        msg = unwords . drop 1 $ ws
        ws  = words xs
parseMessage ('E':xs) = LogMessage (Error lvl) ts msg
  where lvl = read . head $ ws :: Int
        ts  = read . head . drop 1 $ ws :: Int
        msg = unwords . drop 2 $ ws
        ws  = words xs
parseMessage xs = Unknown xs

-- Exercise 2
-- https://en.wikipedia.org/wiki/Binary_search_tree#Insertion
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert lm Leaf = Node Leaf lm Leaf
insert lma@(LogMessage _ tsa _) (Node left lmb@(LogMessage _ tsb _) right )
  | tsa == tsb = Node (Node left lmb Leaf) lma right
  | tsa < tsb = Node (insert lma left) lmb right
  | tsa > tsb = Node left lmb (insert lma right)






{--------------------
 --------------------
 - Initial attempts -
 --------------------
 --------------------}

-- Second attempt with only Prelude functions
-- However, no dot operator
parseMessage' :: String -> LogMessage
parseMessage' ('I':xs) = LogMessage Info ts msg
  where ts  = read (head ws) :: Int
        msg = unwords $ drop 1 ws
        ws  = words xs
parseMessage' ('W':xs) = LogMessage Warning ts msg
  where ts  = read (head ws) :: Int
        msg = unwords $ drop 1 ws
        ws  = words xs
parseMessage' ('E':xs) = LogMessage (Error lvl) ts msg
  where lvl = read (head ws) :: Int
        ts  = read (head (drop 1 ws)) :: Int
        msg = unwords $ drop 2 ws
        ws  = words xs
parseMessage' xs = Unknown xs

-- First attempt:
parseMessage'' :: String -> LogMessage
parseMessage'' ('I':' ':xs) = LogMessage Info ts msg
  where ts  = read (getNextWord xs) :: Int
        msg = dropNextWord xs
parseMessage'' ('W':' ':xs) = LogMessage Warning ts msg
  where ts  = read (getNextWord xs) :: Int
        msg = dropNextWord xs
parseMessage'' ('E':' ':xs) = LogMessage (Error lvl) ts msg
  where lvl = read (getNextWord xs) :: Int
        ts  = read (getNextWord (dropNextWord xs)) :: Int
        msg = dropNextWord $ dropNextWord xs
parseMessage'' xs = Unknown xs

-- reinvented the wheel with the following
-- will instead use lines,words,unwords,take,drop, and (.) -}
getNextWord :: String -> String
getNextWord []         = []
getNextWord (x:" ")    = [x]
getNextWord (x:' ':_)  = [x]
getNextWord (x:xs)     = [x] ++ getNextWord xs

dropNextWord :: String -> String
dropNextWord [] = []
dropNextWord (_:" ") = []
dropNextWord (_:' ':xs) = xs
dropNextWord (_:xs) = dropNextWord xs
