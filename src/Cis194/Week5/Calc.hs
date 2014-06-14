{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Cis194.Week5.Calc where
import Cis194.Week5.ExprT
import Cis194.Week5.Parser
import qualified Cis194.Week5.StackVM as StackVM

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

reify :: ExprT -> ExprT
reify = id

-- Exercise 4

instance Expr ExprT where
  lit x   = Lit x
  add x y = Add x y
  mul x y = Mul x y

-- Integer — works like the original calculator
instance Expr Integer where
  lit x   = x
  add x y = x + y
  mul x y = x * y

-- Bool — every literal value less than or equal to 0 is interpreted as False,
--        and all positive Integers are interpreted as True;
--        “addition” is logical or, “multiplication” is logical and
instance Expr Bool where
  lit x   = x > 0
  add x y = x || y
  mul x y = x && y

-- MinMax — “addition” is taken to be the max function,
--          while “multiplication” is the min function
newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x                     = MinMax x
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

-- Mod7 - all values should be in the ranage 0...6,
--        and all arithmetic is done modulo 7;
--        for example, 5 + 3 = 1.
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit x                 = Mod7 (mod x 7)
  add (Mod7 x) (Mod7 y) = Mod7 (mod (x + y) 7)
  mul (Mod7 x) (Mod7 y) = Mod7 (mod (x * y) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

-- Exercise 5
instance Expr StackVM.Program where
  lit x   = [StackVM.PushI x]
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile s = case (parseExp lit add mul s) of
  Nothing -> Nothing
  (Just program) -> Just program

testCompile :: String -> Maybe (Either String StackVM.StackVal)
testCompile s = case (compile s) of
  Nothing -> Nothing
  (Just program) -> Just (StackVM.stackVM program)

-- Exercise 6
