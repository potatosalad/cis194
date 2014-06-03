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

histogram :: [Integer] -> String
histogram list = (++"==========\n0123456789\n") . occurrenceToRows .
  countOccurrence 0 . map (\x -> (head x, length x)) . group . sort $ list

countOccurrence :: Integer -> [(Integer, Int)] -> [Int]
countOccurrence 10 _ = []
countOccurrence n [] = [0 | _ <- [(n+1)..10]]
countOccurrence n list@((k, count):xs)
  | n == k    = count : countOccurrence (n+1) xs
  | otherwise = 0     : countOccurrence (n+1) list

occurrenceToRows :: [Int] -> String
occurrenceToRows list = case (find (>0) list) of
  Nothing -> ""
  Just _  -> occurrenceToRows (map (+(-1)) list) ++ (map (star) list ++ "\n")
  where star n = if n > 0 then '*' else ' '
