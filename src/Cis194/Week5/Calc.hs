module Cis194.Week5.Calc where
import Cis194.Week5.ExprT

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit x)   = x
eval (Add x y) = (+) (eval x) (eval y)
eval (Mul x y) = (*) (eval x) (eval y)
