import System.IO
import Data.List
import Data.Char
import Debug.Trace

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print . solveProblem $ contents
  hClose handle


mytrace x = traceShow x x

solveProblem :: String -> String
solveProblem = go 0
  where
    go n cs
      | length cs < 14 = show n
      | take 14 cs == nub (take 14 cs) = show $ n+14
      | otherwise                      = go (n+1) (tail cs)