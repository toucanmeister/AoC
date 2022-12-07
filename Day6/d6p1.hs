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
    go n (a:b:c:d:xs)
      | [a,b,c,d] == nub [a,b,c,d] = show $ n+4
      | otherwise                  = go (n+1) (b:c:d:xs)
    go n _ = show n