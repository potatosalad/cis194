module Cis194.Week1 where

-- http://www.seas.upenn.edu/~cis194/hw/01-intro.pdf

------------------------------------
-- Validating Credit Card Numbers --
------------------------------------

-- Exercise 1

toDigits :: Integer -> [Integer]
toDigits x
  | x < 1     = []
  | x < 10    = [x]
  | otherwise = toDigits(div x 10) ++ [(mod x 10)]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse . toDigits $ x

-- Exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = zipWith ($) (cycle z) x
  where z = if length x `mod` 2 == 1 then [id, (*2)] else [(*2), id]

-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits []  = 0
sumDigits (x:xs)
  | x < 1     = sumDigits xs
  | x < 10    = x + (sumDigits xs)
  | otherwise = (sumDigits (toDigits x)) + sumDigits xs

-- Exercise 4

validate :: Integer -> Bool
validate x = ((sumDigits . doubleEveryOther . toDigits $ x) `mod` 10) == 0

-------------------------
-- The Towers of Hanoi --
-------------------------

-- Exercise 5

type Peg  = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi x a b c = hanoi (x-1) a c b ++ [(a, b)] ++ hanoi (x-1) c b a

-- Exercise 6

hanoiK :: Integer -> [Peg] -> [Move]
hanoiK 0 _ = []
hanoiK 1 (a:b:_) = [(a, b)]
hanoiK x (a:b:c:rest) =
  hanoiK k (a:c:b:rest) ++
  hanoiK (x-k) (a:b:rest) ++
  hanoiK k (c:b:a:rest)
  where k = if (null rest) then (x - 1) else (x `div` 2)
hanoiK _ _ = []

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 x a b c d = hanoiK x [a, b, c, d]
