import System.IO
import Data.List
import Data.List.Split ()
import Data.Char
import Debug.Trace
import Data.Array

mytrace x = traceShow x x

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print . solveProblem $ contents
  hClose handle

solveProblem :: String -> String
solveProblem s = 
  let arr = listArray ((1,1),(99,99)) . map digitToInt . filter (not . isSpace) $ s 
  in show . length . filter id . map (isVisible arr) . indices $ arr

isVisible :: Array (Int, Int) Int -> (Int, Int) -> Bool
isVisible arr idx@(x,y)
  | fst idx == 1 || fst idx == 99 || snd idx == 1 || snd idx == 99 = True
  | otherwise = isVisibleDirection arr idx topindices || isVisibleDirection arr idx  bottomindices || isVisibleDirection arr idx leftindices || isVisibleDirection arr idx rightindices
    where
      topindices = [(i,j) | i <- [x], j <- [1..(y-1)]]
      bottomindices = [(i,j) | i <- [x], j <- [(y+1)..99]]
      leftindices = [(i,j) | i <- [1..(x-1)], j <- [y]]
      rightindices = [(i,j) | i <- [(x+1)..99], j <- [y]]

isVisibleDirection arr (x,y) = all (\i -> arr!(x,y) > arr!i)