import System.IO
import Data.List
import Data.List.Split
import Data.Char
import Debug.Trace

mytrace x = traceShow x x

main = do
  handle <- openFile "testinput.txt" ReadMode
  contents <- hGetContents handle
  print . solveProblem $ contents
  hClose handle

data Tree = 
  Directory {
    name :: String,
    children :: [Tree],
    parent :: Tree } | 
  File {
    name :: String,
    size :: Int } |
  Root {
    children :: [Tree]
  }
  deriving (Show)

solveProblem :: String -> String
solveProblem = show . buildTree (Root []) . lines

buildTree :: Tree -> [String] -> Tree
buildTree tree [] = goToRoot tree
buildTree currentdir (line:xs)
  | "$ cd" `isPrefixOf` line = buildTree (changeDir currentdir line) xs
  | "$ ls" `isPrefixOf` line = buildTree currentdir xs
  | "dir" `isPrefixOf` line =  buildTree (addDir currentdir line) xs
  | otherwise = buildTree (addFile currentdir line) xs

changeDir :: Tree -> String -> Tree
changeDir dir line = go dir (drop 5 line)
  where
    go current ".." = parent current
    go current "/" = goToRoot current
    go current targetname = head . filter isTarget $ children current
      where isTarget Directory{name=n}
              | n == targetname = True
              | otherwise       = False
            isTarget File{} = False

goToRoot :: Tree -> Tree
goToRoot r@Root{} = r
goToRoot Directory{parent=p} = p

addDir :: Tree -> String -> Tree
addDir dir@Directory{name=n, children=cs, parent=p} line = 
  Directory n (c:cs) p
    where c = Directory (drop 4 line) [] dir
addDir Root

addFile :: Tree -> String -> Tree
addFile dir line = 
  Directory (name dir) (c:children dir) (parent dir)
    where x = splitOn " " line
          c = File (last x) (read $ head x)

calcStuff :: Tree -> Int
calcStuff _ = 1