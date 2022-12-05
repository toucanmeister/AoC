import System.IO
import Data.List.Split
import Data.Map.Strict (Map, empty, unionWith, insert)
import Data.Char
import Data.Foldable
import Debug.Trace

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print . solveProblem $ contents
  hClose handle

type Crate = Char
type Stack = [Crate]
type StackMap = Map Int Stack

solveProblem :: String -> String
solveProblem s = 
  let
    [initpart, instructionpart] = splitOn "\n\n" s
    stacks = parseStacks initpart
    instructions = parseInstructions instructionpart
  in doInstructions stacks instructions

parseInstructions s = s
doInstructions m s = s

parseStacks :: String -> StackMap
parseStacks s = 
  let
    ls = lines s
    listsToProcess = map (toCrate . mergeDuplicateSplits . splitOn " ") $ init ls
  in foldr (unionWith (++)) empty (map buildStacks (init ls))

mergeDuplicateSplits :: [String] -> [String] -- Empty Columns correspond to 4 "" strings, we merge these
mergeDuplicateSplits = foldr combine (0,[])
  where
    combine "" (3,xs) = (0,"":xs)
    combine "" (n,xs) = (n+1,xs)
    combine s (n,xs) = (0,xs)

toCrate :: [String] -> Maybe Crate
toCrate = map $ parse crateP

buildStacks :: [Maybe Crate] -> StackMap
buildStacks ls = fst $ foldr go (empty, 0) ls
  where
    go :: Maybe Crate -> (StackMap, Int) -> (StackMap, Int)
    go Nothing  (m,k) = (m, k+1)
    go (Just x) (m,k) = (insert k [x] m, k+1)