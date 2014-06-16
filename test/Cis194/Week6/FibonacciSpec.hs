module Cis194.Week6.FibonacciSpec (main, spec) where

import Test.Hspec
import Cis194.Week6.Fibonacci

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fib" $ do
    it "should compute the nth Fibonacci number" $ do
      fib 0 `shouldBe` 0
      fib 1 `shouldBe` 1
      fib 2 `shouldBe` 1
      fib 14 `shouldBe` 377

  describe "fibs1" $ do
    it "should define an infinite list of all Fibonacci numbers" $ do
      take 5 fibs1 `shouldBe` [0, 1, 1, 2, 3]

  describe "fibs2" $ do
    it "should define an infinite list of all Fibonacci numbers" $ do
      take 5 fibs2 `shouldBe` [0, 1, 1, 2, 3]

  describe "streamRepeat" $ do
    it "should define an infinite repeating Stream" $ do
      take 5 (streamToList $ streamRepeat (1::Integer)) `shouldBe` [1, 1, 1, 1, 1]

  describe "streamMap" $ do
    it "should map a function to an infinite Stream" $ do
      take 5 (streamToList . streamMap (*2) $ streamRepeat (1::Integer)) `shouldBe` [2, 2, 2, 2, 2]

  describe "streamFromSeed" $ do
    it "should define an infinite seeded Stream" $ do
      take 5 (streamToList $ streamFromSeed (*2) (1::Integer)) `shouldBe` [1, 2, 4, 8, 16]

  describe "nats" $ do
    it "should define all natural numbers" $ do
      take 5 (streamToList $ nats) `shouldBe` [0, 1, 2, 3, 4]

  describe "ruler" $ do
    it "should correspond to the ruler function" $ do
      take 16 (streamToList $ ruler) `shouldBe` [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4]

  describe "rulerClever" $ do
    it "should correspond to the ruler function" $ do
      take 16 (streamToList $ rulerClever) `shouldBe` [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4]
