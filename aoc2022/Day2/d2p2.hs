import System.IO

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print . solveProblem $ contents
  hClose handle

solveProblem :: String -> Int
solveProblem = sum . map calculateScore . lines
  where
    calculateScore :: String -> Int
    calculateScore "A X" = 3+0
    calculateScore "A Y" = 1+3
    calculateScore "A Z" = 2+6
    calculateScore "B X" = 1+0
    calculateScore "B Y" = 2+3
    calculateScore "B Z" = 3+6
    calculateScore "C X" = 2+0
    calculateScore "C Y" = 3+3
    calculateScore "C Z" = 1+6
    calculateScore _ = 0
