import System.IO
import Data.List
import Data.List.Split
import Data.Char
import Debug.Trace
import qualified Data.Map as Map

mytrace x = traceShow x x

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  putStrLn . solveProblem $ contents
  hClose handle

data Node = Dir Int | File Int
  deriving (Show,Eq)
type Path = [String]
type PathMap = Map.Map Path Node

solveProblem :: String -> String
solveProblem s = show . sum .  filter (<= 100000) . map getSize . filter isDir . Map.elems $ pathmap
  where pathmap = snd .  foldl' buildMap ([],Map.empty) . lines $ s

getSize :: Node -> Int
getSize (Dir n) = n

isDir :: Node -> Bool
isDir (Dir _) = True
isDir (File _) = False

buildMap :: (Path, PathMap) -> String -> (Path, PathMap)
buildMap p s
  | "$ cd "  `isPrefixOf` s = changeDir (drop 5 s) p
  | "$ ls"   `isPrefixOf` s = p
  | "dir " `isPrefixOf` s = addDir (drop 4 s) p
  | otherwise             = addFileAndUpdateDirs s p

changeDir :: String -> (Path, PathMap) -> (Path, PathMap)
changeDir "/" (currentpath,m) = (["/"], m)
changeDir ".." (currentpath,m) = (tail currentpath, m)
changeDir name (currentpath,m) = (name:currentpath, m)

addDir :: String -> (Path, PathMap) -> (Path, PathMap)
addDir name (currentpath,m) = (currentpath, Map.insert path (Dir 0) m)
  where path = name:currentpath

addFileAndUpdateDirs :: String -> (Path, PathMap) -> (Path, PathMap)
addFileAndUpdateDirs s (currentpath, m) = (currentpath, updatedmap)
  where
    updatedmap = updateDirs filesize (currentpath, mapwithfile)
    mapwithfile = addFile filename filesize (currentpath, m)
    filesize = read @Int filesizeString
    [filesizeString, filename] = splitOn " " s

addFile :: String -> Int -> (Path, PathMap) -> PathMap
addFile filename filesize (currentpath, m) = Map.insert path (File filesize) m
  where path = filename:currentpath

updateDirs :: Int -> (Path,PathMap) -> PathMap
updateDirs _ ([], m) = m
updateDirs filesize (currentpath, m) = updateDirs filesize (tail currentpath, updatedmap)
  where
    updatedmap = Map.adjust increaseSize currentpath m
    increaseSize (Dir n) = Dir (n+filesize)