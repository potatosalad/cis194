module Cis194.Week5.Calc where
import Cis194.Week5.ExprT
import Cis194.Week5.Parser

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit x)   = x
eval (Add x y) = (+) (eval x) (eval y)
eval (Mul x y) = (*) (eval x) (eval y)

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr string = case (parseExp Lit Add Mul string) of
  Nothing -> Nothing
  (Just expression) -> Just (eval expression)

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a
