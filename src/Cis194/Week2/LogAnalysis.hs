module Cis194.Week2.LogAnalysis where

import Cis194.Week2.Log

-- http://www.seas.upenn.edu/~cis194/hw/02-ADTs.pdf

-- Exercise 1

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("E":severity:timestamp:message) ->
    LogMessage (Error (read severity)) (read timestamp) (unwords message)
  ("I":timestamp:message) ->
    LogMessage Info (read timestamp) (unwords message)
  ("W":timestamp:message) ->
    LogMessage Warning (read timestamp) (unwords message)
  (_) ->
    Unknown s

parse :: String -> [LogMessage]
parse [] = []
parse s  = map parseMessage (lines s)

-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message@(LogMessage _ timestampA _) tree = case tree of
  (Leaf) -> Node Leaf message Leaf
  (Node _ (Unknown _) _) -> tree
  (Node left root@(LogMessage _ timestampB _) right) -> case timestampA < timestampB of
    False -> Node left root (insert message right)
    True  -> Node (insert message left) root right

-- Exercise 3

build :: [LogMessage] -> MessageTree
build messages = foldl (\tree message -> insert message tree) Leaf messages

-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder tree = case tree of
  (Leaf) -> []
  (Node left (Unknown _) right) -> inOrder(left) ++ inOrder(right)
  (Node left message right) -> inOrder(left) ++ [message] ++ inOrder(right)

-- Exercise 5

isError :: LogMessage -> Bool
isError (LogMessage (Error _) _ _) = True
isError _ = False

isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error severity) _ _) = severity > 49
isSevere _ = False

isRelevant :: LogMessage -> Bool
isRelevant message = isError message && isSevere message

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map (\(LogMessage _ _ message) -> message) $ inOrder $ build $ filter isRelevant messages

-- Exercise 6

-- Answer: Alice (?)
