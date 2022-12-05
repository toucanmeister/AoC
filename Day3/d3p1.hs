import System.IO
import Data.List.Split
import Data.Char

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print . solveProblem $ contents
  hClose handle

solveProblem :: String -> Int
solveProblem = sum . map solveLine . lines

solveLine :: String -> Int
solveLine = assignValue . findDoubleLetter

assignValue :: Char -> Int
assignValue c
  | isUpper c = ord c - 38
  | isLower c = ord c - 96
  | otherwise = 0
  

findDoubleLetter :: String -> Char
findDoubleLetter s = 
  let [a,b] = take 2 . splitInTwo $ s
  in head [x | x <- a, x `elem` b]
  where splitInTwo x = chunksOf (length x `div` 2) x