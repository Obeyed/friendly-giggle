{-# OPTIONS_GHC -Wall #-}

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
evalStr s = maybeEval (Parser.parseExp Lit Add Mul s)
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
  lit a   = Lit a
  mul a b = Mul a b
  add a b = Add a b

-- constrain the type of the arguments to ExprT
reify :: ExprT -> ExprT
reify = id

-- Exercise 4
instance Expr Integer where
  lit a   = a
  mul a b = a * b
  add a b = a + b

instance Expr Bool where
  lit a | a < 1 = False
        | otherwise = True
  add a b = a || b
  mul a b = a && b

newtype MinMax = MinMax Integer deriving (Ord, Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit a   = MinMax a
  add a b = max a b
  mul a b = min a b

instance Expr Mod7 where
  lit a = Mod7 (mod a 7)
  add (Mod7 a) (Mod7 b) = lit (mod (add a b :: Integer) 7)
  mul (Mod7 a) (Mod7 b) = lit (mod (mul a b :: Integer) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4) + 5"
-- the following will print warning because of missing signatures,
-- but we don't need any..
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
