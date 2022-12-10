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

type Position = (Int,Int)
type AllPosition = [Position]
type PositionLog = [Position]
type Instruction = AllPosition -> AllPosition
type PositionWriter = Writer PositionLog AllPosition

solveProblem :: String -> String
solveProblem s = show . length . nub $ positionlog
  where
    positionlog = snd . runWriter $ doInstructions start instructions
    start = replicate 10 (0,0)  
    instructions = parseInstructions s

doInstructions :: AllPosition -> [Instruction] -> PositionWriter
doInstructions = foldM runInstrW

parseInstructions :: String -> [Instruction]
parseInstructions = concatMap toInstructions . lines
  where
    toInstructions :: String -> [Instruction]
    toInstructions ('U':' ':rest) = replicate (read @Int rest) atomUp
    toInstructions ('D':' ':rest) = replicate (read @Int rest) atomDown
    toInstructions ('L':' ':rest) = replicate (read @Int rest) atomLeft
    toInstructions ('R':' ':rest) = replicate (read @Int rest) atomRight


runInstrW :: AllPosition -> Instruction -> PositionWriter
runInstrW pos f = do
  tell [last $ f pos]
  return (f pos)

moveTails :: Position -> AllPosition -> AllPosition
moveTails newhead (t:ts) = t' : moveTails t' ts
  where t' = movetail newhead t
moveTails neahead [] = []

atomUp :: Instruction
atomUp ((x,y):ts) = newhead : moveTails newhead ts
  where newhead = (x,y+1)
atomDown :: Instruction
atomDown ((x,y):ts) = newhead : moveTails newhead ts
  where newhead = (x,y-1)
atomLeft :: Instruction
atomLeft ((x,y):ts) = newhead : moveTails newhead ts
  where newhead = (x-1,y)
atomRight :: Instruction
atomRight ((x,y):ts) = newhead : moveTails newhead ts
  where newhead = (x+1,y)

movetail :: Position -> Position -> Position
movetail (hx,hy) (tx,ty)
  | hx == tx && (hy - ty) > 1  = (tx, ty+1)
  | hx == tx && (hy - ty) < -1 = (tx, ty-1)
  | hy == ty && (hx - tx) > 1  = (tx+1, ty)
  | hy == ty && (hx - tx) < -1 = (tx-1, ty)
  | (hx - tx) > 1 && (hy - ty) >= 1   = (tx+1, ty+1)
  | (hx - tx) >= 1 && (hy - ty) > 1   = (tx+1, ty+1)
  | (hx - tx) > 1 && (hy - ty) <= -1  = (tx+1, ty-1)
  | (hx - tx) >= 1 && (hy - ty) < -1  = (tx+1, ty-1)
  | (hx - tx) < -1 && (hy - ty) >= 1  = (tx-1, ty+1)
  | (hx - tx) <= -1 && (hy - ty) > 1  = (tx-1, ty+1)
  | (hx - tx) < -1 && (hy - ty) <= -1 = (tx-1, ty-1)
  | (hx - tx) <= -1 && (hy - ty) < -1 = (tx-1, ty-1)
  | otherwise = (tx,ty)
