module Cis194.Week4 where

-- Exercise 1: Wholemeal programming

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (+(-2)) . filter (even)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter(even) . takeWhile (>1) . iterate (fun2'')
  where fun2'' x = if even x then div x 2 else 3 * x + 1

-- Exercise 2: Folding with trees

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: Ord a => [a] -> Tree a
foldTree = foldr avlInsert Leaf

balanceFactor :: Tree a -> Integer
balanceFactor = avlBalanceFactor

height :: Tree a -> Integer
height = avlHeight

-- AVL Tree
-- http://en.wikipedia.org/wiki/AVL_tree

avlInsert :: Ord a => a -> Tree a -> Tree a
avlInsert k Leaf = Node 0 Leaf k Leaf
avlInsert k (Node _ l k1 r) =
  if k <= k1
  then let l' = avlInsert k l in
    avlBalance (Node (avlMaxHeight l' r) l' k1 r)
  else let r' = avlInsert k r in
    avlBalance (Node (avlMaxHeight l r') l k1 r')

avlHeight :: Tree a -> Integer
avlHeight Leaf = 0
avlHeight (Node h _ _ _) = h

avlMaxHeight :: Tree a -> Tree a -> Integer
avlMaxHeight a b = 1 + max (avlHeight a) (avlHeight b)

avlBalance :: (Ord a) => Tree a -> Tree a
avlBalance Leaf = Leaf
avlBalance t@(Node _ l k r)
  | abs (avlBalanceFactor t) < 2 = t
  | avlHeight l < avlHeight r =
    case r of
      Leaf -> error "cannot rotate a leaf"
      (Node _ l1 k1 r1) ->
        let child = (Node (avlMaxHeight l l1) l k l1) in
          (Node (avlMaxHeight child r1) child k1 r1)
  | otherwise =
    case l of
      Leaf -> error "cannot rotate a leaf"
      (Node _ l1 k1 r1) ->
        let child = (Node (avlMaxHeight r1 r) r1 k r) in
          (Node (avlMaxHeight l1 child) l1 k1 child)

avlBalanceFactor :: Tree a -> Integer
avlBalanceFactor Leaf = 0
avlBalanceFactor (Node _ l _ r) = avlHeight l - avlHeight r

-- Exercise 3: More folds!

xor :: [Bool] -> Bool
xor = foldr (/=) False
