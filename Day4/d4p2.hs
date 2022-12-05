import System.IO
import Data.List.Split
import Data.List
import Data.Char
import Debug.Trace

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print . solveProblem $ contents
  hClose handle

type Assignment = (Int,Int)

solveProblem :: String -> Int
solveProblem = length . filter isPartialCover . fmap (toAssignments . splitOn ",") . lines

toAssignments :: [String] -> (Assignment, Assignment)
toAssignments l = (go $ head l, go $ l!!1)
  where 
    go = toAssignment . makeTwoElemList
    makeTwoElemList s = map (read @Int) $ splitOn "-" s
    toAssignment twoElemList = (head twoElemList, twoElemList!!1)

isPartialCover :: (Assignment, Assignment) -> Bool
isPartialCover ((s1,e1), (s2,e2)) 
  | e2 < s1   = False
  | e1 < s2   = False
  | otherwise = True