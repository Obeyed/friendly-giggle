{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

{- Second homework http://www.seas.upenn.edu/~cis194/spring13/hw/02-ADTs.pdf -}

-- Exercise 1

parseMessage :: String -> LogMessage
parseMessage ('I':' ':xs) = LogMessage Info ts msg
  where ts  = read (getNextWord xs) :: Int
        msg = dropNextWord xs
parseMessage ('W':' ':xs) = LogMessage Warning ts msg
  where ts  = read (getNextWord xs) :: Int
        msg = dropNextWord xs
parseMessage ('E':' ':xs) = LogMessage (Error lvl) ts msg
  where lvl = read (getNextWord xs) :: Int
        ts  = read (getNextWord (dropNextWord xs)) :: Int
        msg = dropNextWord $ dropNextWord xs
parseMessage xs = Unknown xs

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
