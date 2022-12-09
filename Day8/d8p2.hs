import System.IO
import Data.List
import Data.List.Split
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
  in show . maximum . mytrace . map (scenicScore arr) . indices $ arr

scenicScore :: Array (Int, Int) Int -> (Int, Int) -> Int
scenicScore arr idx@(x,y)
  | fst idx == 1 || fst idx == 99 || snd idx == 1 || snd idx == 5 = 99
  | otherwise = scenicScoreDirection arr idx topindices * scenicScoreDirection arr idx bottomindices * scenicScoreDirection arr idx leftindices * scenicScoreDirection arr idx rightindices
    where
      topindices = [(i,j) | i <- [(x-1),(x-2)..1], j <- [y]]
      bottomindices = [(i,j) | i <- [(x+1)..99], j <- [y]]
      leftindices = [(i,j) | i <- [x], j <- [(y-1),(y-2)..1]]
      rightindices = [(i,j) | i <- [x], j <- [(y+1)..99]]

scenicScoreDirection arr idx =  length . takeWhileOneMore isSmaller
  where
    takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []
    isSmaller i = arr!idx > arr!i