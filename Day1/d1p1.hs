import Control.Monad
import Data.Char
import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    print . solveProblem $ contents
    hClose handle

solveProblem :: String -> Int
solveProblem = maximum . sumByElf . map toInt . lines

toInt :: String -> Int
toInt "" = 0
toInt s  = read s

sumByElf :: [Int] -> [Int]
sumByElf input = 
    let (x,l) = foldr combine (0,[]) input
    in x:l
    where combine 0 (acc,l) = (0,acc:l)
          combine n (acc,l) = (acc+n,l)

