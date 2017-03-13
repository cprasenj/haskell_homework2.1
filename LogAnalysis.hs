module LogAnalysis where
import Log
import Data.List
import Data.List.Split
import Data.Map ()
import qualified Data.Map as Map

readValFromList :: [String] -> Int -> Int
readValFromList inputList position = read $ head $ drop position inputList

parseMessage :: String -> LogMessage
parseMessage aLine
  |messageType `elem` ["I", "W"] =
    LogMessage (types Map.! messageType) (readValFromList splittedLine 1) (unwords (drop 2 splittedLine))
  |(==) "E" $ head splittedLine =
    LogMessage (Error $ readValFromList splittedLine 1) (readValFromList splittedLine 2) (unwords (drop 3 splittedLine))
  |otherwise = Unknown aLine
  where splittedLine = splitOn " " aLine
        types = Map.fromList [("I", Info), ("W", Warning)]
        messageType = head splittedLine

parse :: String -> [LogMessage]
parse fileString = map parseMessage linesOfFile
  where linesOfFile = lines fileString

insertInto :: MessageTree -> LogMessage -> MessageTree
insertInto tree (Unknown _) = tree
insertInto Leaf x = Node Leaf x Leaf
insertInto (Node t1 (LogMessage a b c) t2) (LogMessage p q r)
 |(<) b q = Node t1 (LogMessage a b c) (insertInto t2 (LogMessage p q r))
 |otherwise = Node (insertInto t1 (LogMessage p q r)) (LogMessage a b c) t2

build :: [LogMessage] -> MessageTree
build = foldl insertInto Leaf

buildLogTree :: String -> MessageTree
buildLogTree = build . parse

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node t1 x t2) = inOrder t1 ++ [x] ++ inOrder t2

relevantErrorMessage :: LogMessage -> String
relevantErrorMessage (Unknown _) = ""
relevantErrorMessage (LogMessage Info _ _) = ""
relevantErrorMessage (LogMessage Warning _ _) = ""
relevantErrorMessage (LogMessage (Error siviarity) _ message)
  | (>=) siviarity 50 = message
  | otherwise = ""

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMessages = filter (not . Data.List.null) $ map relevantErrorMessage logMessages
