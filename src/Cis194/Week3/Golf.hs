module Cis194.Week3.Golf where

import Data.List

-- http://www.seas.upenn.edu/~cis194/hw/03-rec-poly.pdf

-- Exercise 1: Hopscotch

-- http://stackoverflow.com/a/2028218/818187
every :: [a] -> Int -> [a]
every xs n = case drop n xs of
  []     -> []
  (y:ys) -> y : every ys n

skips :: [a] -> [[a]]
skips [] = []
skips x@(_:_) = map (every x) [0..length x - 1]

-- Exercise 2: Local maxima

localMaxima :: [Integer] -> [Integer]
localMaxima (a:x:b:xs)
  | a < x && b < x = [x] ++ localMaxima (b:xs)
  | otherwise      = localMaxima (x:b:xs)
localMaxima _ = []

-- Exercise 3: Histogram

-- TODO: write my own implementation once I understand this
-- https://github.com/coopernurse/cis194/blob/6fb94ee791932594ae9aa595656158c36e06975b/src/Cis194/Hw/Golf.hs#L33-L55
histogram :: [Integer] -> String
histogram = (++"==========\n0123456789\n") . toRows . digitFreqs

digitFreqs :: [Integer] -> [Int]
digitFreqs l1 = digitFreqs' 0 $ sort l1
  where digitFreqs' n l2
          | n == 10 = []
          | otherwise = case (span (==n) l2) of
          (left, right) -> length left : digitFreqs' (n+1) right

toRows :: [Int] -> String
toRows list = case (find (>0) list) of
  Nothing -> ""
  Just _  -> toRows (map (+(-1)) list) ++ (map (mark) list ++ "\n")
  where mark n = if n > 0 then '*' else ' '
