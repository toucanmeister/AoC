import System.IO
import Data.List.Split
import Data.Char

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print . solveProblem $ contents
  hClose handle

solveProblem :: String -> Int
solveProblem = sum . map solveGroup . chunksOf 3 . lines

solveGroup :: [String] -> Int
solveGroup = assignValue . head . findCommonLetters

assignValue :: Char -> Int
assignValue c
  | isUpper c = ord c - 38
  | isLower c = ord c - 96
  | otherwise = 0

findCommonLetters :: [String] -> [Char]
findCommonLetters list = [c | c <- head list, all (c `elem`) list]