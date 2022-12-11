import System.IO
import Data.List
import Data.List.Split
import Data.Char
import Debug.Trace
import Control.Monad.Writer
import Data.Array

mytrace x = traceShow x x

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  putStrLn . solveProblem $ contents
  hClose handle

data CPUState = CPUState {
  xstate :: Int,  
  cpucycle :: Int,
  crtpos :: (Int,Int)
} deriving (Show)
type Instruction = CPUState -> CPUState

solveProblem :: String -> String
solveProblem s = showArray . buildScreenArray . mytrace . completeLog $ log
  where 
    start = CPUState{xstate=1, cpucycle=1, crtpos=(0,0)}
    log = doInstructions start . map parseInstruction . lines $ s

showArray :: Array (Int,Int) Char -> String
showArray arr = concatMap (showLine arr) [0..5]
  where
    showLine arr lineIndex = map (showEntry arr lineIndex) [0..39] ++ "\n"
    showEntry arr lineIndex colIndex = arr!(lineIndex, colIndex)

buildScreenArray :: [CPUState] -> Array (Int,Int) Char
buildScreenArray = foldr updateArray start
  where start = listArray ((0,0), (5,39)) (replicate (6*40) '.')

updateArray :: CPUState -> Array (Int,Int) Char -> Array (Int,Int) Char
updateArray CPUState{xstate=x, crtpos=(i,j)} arr
  | (x-1) == j || x == j || (x+1) == j = arr // [((i,j), '#')]
  | otherwise                          = arr

completeLog :: [CPUState] -> [CPUState]
completeLog [a] = [a]
completeLog (a@CPUState{xstate=x, cpucycle=c1, crtpos=pos} : b@CPUState{cpucycle=c2} : xs)
  | c2 == c1+1 = a : completeLog (b:xs)
  | otherwise  = a : CPUState{xstate=x, cpucycle = c1+1, crtpos = updateCrtPos pos} : completeLog (b:xs)

doInstructions :: CPUState -> [Instruction] -> [CPUState]
doInstructions state ins = reverse $ scanr ($) state (reverse ins)

parseInstruction :: String -> Instruction
parseInstruction "noop" = noop
parseInstruction ins = addx n
  where n = read @Int (drop 5 ins)

noop :: Instruction
noop CPUState{xstate=x, cpucycle=c, crtpos=pos} = CPUState{xstate=x, cpucycle = c+1, crtpos = updateCrtPos pos}

addx :: Int -> Instruction
addx n CPUState{xstate=x, cpucycle=c, crtpos=pos} =  CPUState{xstate=x+n, cpucycle = c+2, crtpos = updateCrtPos . updateCrtPos $ pos}

updateCrtPos :: (Int,Int) -> (Int,Int)
updateCrtPos (x,y)
  | y < 39    = (x,y+1)
  | x == 5    = (0,0)
  | otherwise = (x+1,0)