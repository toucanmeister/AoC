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
solveProblem = length . filter isFullCover . fmap (toAssignments . splitOn ",") . lines

toAssignments :: [String] -> (Assignment, Assignment)
toAssignments l = (go $ head l, go $ l!!1)
  where 
    go = toAssignment . makeTwoElemList
    makeTwoElemList s = map (read @Int) $ splitOn "-" s
    toAssignment twoElemList = (head twoElemList, twoElemList!!1)

isFullCover :: (Assignment, Assignment) -> Bool
isFullCover ((s1,e1), (s2,e2)) 
  | s1 <= s2 && e2 <= e1 = True
  | s2 <= s1 && e1 <= e2 = True
  | otherwise            = False