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
insert lm (Node left lm' right)
  | tsa == tsb = Node (Node left lm' Leaf) lm right
  | tsa < tsb = Node (insert lm left) lm' right
  | tsa > tsb = Node left lm' (insert lm right)
  where
    tsa = getLogMessageTimestamp lm
    tsb = getLogMessageTimestamp lm'
insert _ _ = error "Something was not right"

getLogMessageTimestamp :: LogMessage -> TimeStamp
getLogMessageTimestamp (LogMessage _ ts _) = ts
getLogMessageTimestamp (Unknown _ ) = error "No timestamp for Unknowns"

-- Exercise 3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (lm:lms) = insert lm $ build lms

-- Exercise 4
-- https://en.wikipedia.org/wiki/Binary_search_tree#Traversal
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf          = []
inOrder (Node l lm r) = inOrder l ++ [lm] ++ inOrder r

-- Exercise 5
-- test it in GHCi with
--    testWhatWentWrong parse whatWentWrong "error.log"
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong lms = (getCriticalErrors . inOrder . build) lms
  where
    getCriticalErrors [] = []
    getCriticalErrors ((LogMessage (Error lvl) _ msg):ls) =
      if lvl > 50
      then [msg] ++ getCriticalErrors ls
      else [] ++ getCriticalErrors ls
    getCriticalErrors (_:ls) = getCriticalErrors ls


{- Final output was:
 - [
 - "Mustardwatch opened, please close for proper functioning!",
 - "All backup mustardwatches are busy",
 - "Depletion of mustard stores detected!",
 - "Hard drive failure: insufficient mustard",
 - "Twenty seconds remaining until out-of-mustard condition",
 - "Ten seconds remaining until out-of-mustard condition",
 - "Empty mustard reservoir! Attempting to recover...",
 - "Recovery failed! Initiating shutdown sequence"
 - ]
 - }



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
