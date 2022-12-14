import System.IO
import Data.List
import Data.List.Split
import Data.Char
import Debug.Trace ( traceShow , trace )
import Data.Either
import Data.Maybe
import Data.Array

mytrace x = traceShow x x

main = do
  handle <- openFile "testinput.txt" ReadMode
  contents <- hGetContents handle
  putStrLn . solveProblem $ contents
  hClose handle

type Coord = (Int,Int)
type Heightmap = Array Coord Char
type Visitedmap = Array Coord Bool
type Distancemap = Array Coord Int
type Path = [Coord]

solveProblem :: String -> String
solveProblem s = show $ shortestPath heights start
  where
    start = findStart heights
    heights = toArray s

chardiff :: Char -> Char -> Int
chardiff a b = ord a - ord b

shortestPath :: Heightmap -> Coord -> (Path,Distancemap)
shortestPath heights start = 
  let
    visits = array (bounds heights) [((i,j), False) | i <- [xlow..xhigh], j <- [ylow..yhigh]] -- Array to keep track of where we've visited
    distances = array (bounds heights) [((i,j), 1000000) | i <- [xlow..xhigh], j <- [ylow..yhigh]] -- Array to keep track of distances to the goal we've computed
    ((xlow, ylow), (xhigh, yhigh)) = bounds heights
  in bfs [start] heights visits distances

bfs :: [Coord] -> Heightmap -> Visitedmap -> Distancemap -> Path
bfs [] heights visits distances = ([],distances)
bfs (pos:queue) heights visits distances
  | heights!pos == 'E' = ([],distances//[(pos,0)])
  | otherwise = (pos:path, newdistances)
    where
      (path, _) = bfs newqueue heights newvisits newdistances
      newqueue = queue ++ possibleDirections heights visits pos
      newvisits = visits//[(pos,True)]
      newdistances = distances//[(pos, distances!pos + 1)]

possibleDirections :: Heightmap -> Visitedmap -> Coord -> [Coord]
possibleDirections heights visited (x,y) = filter isPossible [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
  where
    isPossible pos
      | isInBounds heights pos && not (visited!pos) && (heights!(x,y)) == 'S' = True
      | isInBounds heights pos && not (visited!pos) = if (heights!pos) /= 'E' then (heights!pos) `chardiff` (heights!(x,y)) <= 1 else 'z' `chardiff` (heights!(x,y)) <= 1
      | otherwise = False

isInBounds :: Heightmap -> Coord -> Bool
isInBounds heights (x,y) = x >= xlow && x <= xhigh && y >= ylow && y <= yhigh
  where
    ((xlow, ylow), (xhigh, yhigh)) = bounds heights

findStart :: Heightmap -> Coord
findStart = fst . head . filter ((=='S') . snd) . assocs

toArray :: String -> Heightmap
toArray s = listArray ((0,0),(bound_x,bound_y)) . filter (not . isSpace) $ s
  where
    bound_x = length ls - 1
    bound_y = length(head ls) - 1
    ls = lines s

showArray :: Array (Int,Int) Char -> String
showArray arr = concatMap (showLine arr) [0..bound_x]
  where
    showLine arr lineIndex = map (showEntry arr lineIndex) [0..bound_y] ++ "\n"
    showEntry arr lineIndex colIndex = arr!(lineIndex, colIndex)
    (bound_x, bound_y)= snd . bounds $ arr