import System.IO
import Data.List
import Data.List.Split
import Data.Char
import Debug.Trace
import Control.Monad.Writer

mytrace x = traceShow x x

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print . solveProblem $ contents
  hClose handle

type CPUState = (XState, Cycle)
type XState = Int
type Cycle = Int
type Instruction = CPUState -> CPUState

solveProblem :: String -> String
solveProblem s = show . sum . computeInterestingValues $ log
  where log = doInstructions (1,1) . map parseInstruction . lines $ s

computeInterestingValues :: [CPUState] -> [Int]
computeInterestingValues log = map (nextInterestingValue log) [20,60..220]

nextInterestingValue :: [CPUState] -> Int -> Int
nextInterestingValue log n = n*state
  where (state,cycle) = last . takeWhile ((<=n) . snd) $ log

doInstructions :: CPUState -> [Instruction] -> [CPUState]
doInstructions state ins = reverse $ scanr ($) state (reverse ins)

parseInstruction :: String -> Instruction
parseInstruction "noop" = \(x,c) -> (x,c+1)
parseInstruction ins = \(x,c) -> (x + n, c+2)
  where n = read @Int (drop 5 ins)