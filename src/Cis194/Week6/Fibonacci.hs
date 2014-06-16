module Cis194.Week6.Fibonacci where

import Data.Bits

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = 0:1:zipWith (+) fibs2 (tail fibs2)

-- Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = (x:streamToList xs)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed $ streamFromSeed f (f seed)

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = interleaveStreams odds evens
  where
    evens = streamMap thomae . streamMap (*2) $ streamTail nats
    odds  = streamRepeat 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

streamTail :: Stream a -> Stream a
streamTail (Cons _ xs) = xs

thomae :: Integer -> Integer
thomae n
  | odd n = 0
  | otherwise = round . logBase 2 $ fromIntegral $ n .&. (-n)

rulerClever :: Stream Integer
rulerClever = rulerClever' 0
  where rulerClever' n = interleaveStreams (streamRepeat n) (rulerClever' (n + 1))

-- Exercise 6 (optional)
--x :: Stream Integer
--x = Cons 0 $ Cons (1 * x) $ streamRepeat 0
----x = Cons 0 (Cons (1 * x) (streamMap (\p -> (0 * (x ^ p))) (streamTail (streamTail nats))))

--instance Num Stream Integer where
--  fromInteger n = Cons n $ streamMap (\p -> (0 * (x ^ p))) nats

-- Exercise 7 (optional)
