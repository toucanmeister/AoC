import System.IO
import Data.List.Split
import Data.Map.Strict qualified as M (Map, empty, unionWith, insert, lookup, adjust, toAscList)
import Data.Char
import Data.Maybe (fromJust)
import Data.Foldable 
import Debug.Trace

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print . solveProblem $ contents
  hClose handle

type Crate = Char
type Stack = [Crate]
type StackMap = M.Map Int Stack
type Instruction = StackMap -> StackMap

solveProblem :: String -> String
solveProblem s = 
  let
    [initpart, instructionpart] = splitOn "\r\n\r\n" s
    stacks = parseStacks initpart
    instructions = parseInstructions instructionpart
    finalStack = doInstructions instructions stacks
  in show . getTopCrates $ finalStack


doInstructions :: [Instruction] -> StackMap -> StackMap
doInstructions = foldr (.) id . reverse

getTopCrates :: StackMap -> [Crate]
getTopCrates = map (head . snd) . M.toAscList

-- Parsing Instructions
parseInstructions :: String -> [Instruction]
parseInstructions = map (toInstruction . toTuple) . lines

toTuple :: String -> (Int,Int,Int)
toTuple s = 
  let ws = words s
  in (read $ ws!!1, read $ ws!!3, read $ ws!!5)

toInstruction :: (Int,Int,Int) -> Instruction
toInstruction (n, from, to) m = 
  let 
    c = take n . fromJust $ M.lookup from m
    inserted = M.adjust (c++) to m
  in M.adjust (drop n) from inserted

mytrace x = traceShow x x

-- Parsing Stacks
parseStacks :: String -> StackMap
parseStacks s = 
  let 
    lists = map (toCrates . map removeTrailingSpace . mergeDuplicateSplits . splitOn " ") . init . lines $ s
    stacks = map buildStacks lists
  in foldr (M.unionWith (++)) M.empty stacks

removeTrailingSpace :: String -> String
removeTrailingSpace = reverse . dropWhile isSpace . reverse

mergeDuplicateSplits :: [String] -> [String] -- Empty Columns correspond to 4 "" strings, we merge these
mergeDuplicateSplits = snd . foldr combine (0,[])
  where
    combine "" (3,xs) = (0,"":xs)
    combine "" (n,xs) = (n+1,xs)
    combine s (n,xs) = (0,s:xs)

toCrates :: [String] -> [Maybe Crate]
toCrates = map toCrate
  where
    toCrate ('[':x:"]") = Just x
    toCrate _ = Nothing

buildStacks :: [Maybe Crate] -> StackMap
buildStacks = fst . foldl' go (M.empty, 1)
  where
    go :: (StackMap, Int) -> Maybe Crate -> (StackMap, Int)
    go (m,k) Nothing  = (m, k+1)
    go (m,k) (Just x) = (M.insert k [x] m, k+1)
