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


-- Exercise 3
{-
 - Create a type class called Expr with three methods called
 - lit, add, and mul which parallel the constructors of ExprT.
 -}
class Expr a where
  add, mul :: a -> a -> a
  lit :: Integer -> a

instance Expr ExprT where
  lit a = Lit a
  mul a b = Mul a b
  add a b = Add a b

instance Expr Integer where
  lit a = a
  mul a b = a * b
  add a b = a + b

-- constrain the type of the arguments to ExprT
reify :: ExprT -> ExprT
reify = id

