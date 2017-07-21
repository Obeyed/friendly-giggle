module Calc where

import ExprT
import Parser

-- Exercise 1
{-
 - 1st version of the calculator
 -}
eval :: ExprT -> Integer
eval (Mul a b) = eval a * eval b
eval (Add a b) = eval a + eval b
eval (Lit a)   = a


-- Exercise 2
{-
 - Leverage the assets of the UI team to implement the value-added
 - function evalStr, which evaluates arithmetic expressions given as a String
 -}
evalStr :: String -> Maybe Integer
evalStr s = maybeEval (parseExp Lit Add Mul s)
  where maybeEval (Just a) = Just (eval a)
        maybeEval Nothing  = Nothing
