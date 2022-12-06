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
    calculateScore "A X" = 1+3
    calculateScore "A Y" = 2+6
    calculateScore "A Z" = 3+0
    calculateScore "B X" = 1+0
    calculateScore "B Y" = 2+3
    calculateScore "B Z" = 3+6
    calculateScore "C X" = 1+6
    calculateScore "C Y" = 2+0
    calculateScore "C Z" = 3+3
    calculateScore _ = 0
