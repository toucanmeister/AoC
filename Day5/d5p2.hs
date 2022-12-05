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
