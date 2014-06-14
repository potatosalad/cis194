module Cis194.Week5.CalcSpec (main, spec) where

import Test.Hspec
import Cis194.Week5.Calc
import Cis194.Week5.ExprT
import qualified Cis194.Week5.StackVM as StackVM

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Calculator - eval" $ do
    it "should add and multiply numbers" $ do
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

  describe "Calculator - evalStr" $ do
    it "should evaluate well formed strings" $ do
      evalStr "(2+3)*4" `shouldBe` Just 20
    it "evals the multiplication operator before addition" $ do
      evalStr "2+3*4" `shouldBe` Just 14
    it "returns Nothing if the string is malformed" $ do
      evalStr "2+3*" `shouldBe` Nothing

  describe "Expr ExprT" $ do
    it "should generate ExprT expression" $ do
      (mul (add (lit 2) (lit 3)) (lit 4)) `shouldBe` (Mul (Add (Lit 2) (Lit 3)) (Lit 4))

  describe "Expr Integer" $ do
    it "should calculate integer value" $ do
      (mul (add (lit 2) (lit 3)) (lit 4)) `shouldBe` (20::Integer)

  describe "Expr Bool" $ do
    it "should treat positive literals as True" $ do
      (lit 3) `shouldBe` True

    it "should treat negative literals as False" $ do
      (lit (-2)) `shouldBe` False

    it "should treat additon as a logical OR" $ do
      (add (lit 1) (lit 1)) `shouldBe` True
      (add (lit (-1)) (lit 1)) `shouldBe` True
      (add (lit 1) (lit (-1))) `shouldBe` True
      (add (lit (-1)) (lit (-1))) `shouldBe` False

    it "should treat multiplicsation as a logical AND" $ do
      (mul (lit 1) (lit 1)) `shouldBe` True
      (mul (lit (-1)) (lit 1)) `shouldBe` False
      (mul (lit 1) (lit (-1))) `shouldBe` False
      (mul (lit (-1)) (lit (-1))) `shouldBe` False

    it "should calculate integer value" $ do
      (mul (add (lit 2) (lit 3)) (lit 4)) `shouldBe` True

  describe "Expr MinMax" $ do
    it "should return integer for lit" $ do
      (lit 4) `shouldBe` (MinMax 4)

    it "should return the largest for add" $ do
      (add (lit 4) (lit 8)) `shouldBe` (MinMax 8)

    it "should return the smallest for mul" $ do
      (mul (lit 4) (lit 8)) `shouldBe` (MinMax 4)

  describe "Expr Mod7" $ do
    it "should return integer for within the range 0-7" $ do
      (lit 4) `shouldBe` (Mod7 4)
      (lit 8) `shouldBe` (Mod7 1)
      (lit (-2)) `shouldBe` (Mod7 5)

    it "should perform addition and multiplication modulo 7" $ do
      (add (lit 6) (lit 9)) `shouldBe` (Mod7 1)
      (mul (lit 4) (lit 10)) `shouldBe` (Mod7 5)

  describe "testExp" $ do
    it "should be Just (-7) for Integer" $ do
      (testExp :: Maybe Integer) `shouldBe` (Just (-7))

    it "should be Just True for Bool" $ do
      (testExp :: Maybe Bool) `shouldBe` (Just True)

    it "should be Just (MinMax 5) for MinMax" $ do
      (testExp :: Maybe MinMax) `shouldBe` (Just (MinMax 5))

    it "should be Just (Mod7 0) for Mod7" $ do
      (testExp :: Maybe Mod7) `shouldBe` (Just (Mod7 0))

  describe "compile" $ do
    it "should be Just [PushI 3,PushI (-4),Mul,PushI 5,Add] for (3 * -4) + 5" $ do
      (compile "(3 * -4) + 5") `shouldBe` (Just [StackVM.PushI 3,StackVM.PushI (-4),StackVM.Mul,StackVM.PushI 5,StackVM.Add])

  describe "testCompile" $ do
    it "should be Just (Right (IVal (-7))) for (3 * -4) + 5" $ do
      (testCompile "(3 * -4) + 5") `shouldBe` (Just (Right (StackVM.IVal (-7))))

  describe "withVars" $ do
    it "should be Just 9 for expression" $ do
      (withVars [("x", 6)] $ add (lit 3) (var "x")) `shouldBe` (Just 9)

    it "should be Nothing for expression" $ do
      (withVars [("x", 6)] $ add (lit 3) (var "y")) `shouldBe` Nothing

    it "should be Just 54 for expression" $ do
      (withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))) `shouldBe` (Just 54)
