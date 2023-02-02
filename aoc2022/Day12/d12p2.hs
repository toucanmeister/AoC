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
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  putStrLn . solveProblem $ contents
  hClose handle

type Coord = (Int,Int)
type Heightmap = Array Coord Char
type Visitedmap = Array Coord Bool
type Distancemap = Array Coord Int
type Path = [Coord]

solveProblem :: String -> String
solveProblem s = show . minimum . filter (>0) $ distances
  where
    distances = map ((! goal) . shortestPath heights) starts
    starts = findAllInArr heights 'a'
    goal = findInArr heights 'E'
    heights = toArray s

chardiff :: Char -> Char -> Int
chardiff a b = ord a - ord b

shortestPath :: Heightmap -> Coord -> Distancemap
shortestPath heights start = 
  let
    visits_init = array (bounds heights) [((i,j), False) | i <- [xlow..xhigh], j <- [ylow..yhigh]] -- Array to keep track of where we've visited
    visits = visits_init//[(start,True)] -- Declare the starting point as visited
    distances = array (bounds heights) [((i,j), 0) | i <- [xlow..xhigh], j <- [ylow..yhigh]] -- Array to keep track of distances to the goal we've computed
    ((xlow, ylow), (xhigh, yhigh)) = bounds heights
  in bfs [start] heights visits distances

bfs :: [Coord] -> Heightmap -> Visitedmap -> Distancemap -> Distancemap
bfs [] _ _ distances = distances
bfs (pos:queue) heights visits distances
  | heights!pos == 'E' = distances
  | otherwise          = bfs newqueue heights newvisits newdistances
    where
      newvisits = visits//[(p, True) | p <- p_directions]
      newdistances = distances//[(p, currentdistance+1) | p <- p_directions]
      currentdistance = distances!pos
      newqueue = queue ++ p_directions
      p_directions = possibleDirections heights visits pos

possibleDirections :: Heightmap -> Visitedmap -> Coord -> [Coord]
possibleDirections heights visited (x,y) = filter isPossible [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
  where
    isPossible pos
      | isInBounds heights pos && not (visited!pos) && (heights!(x,y)) == 'S' = heights!pos `chardiff` 'a' <= 1
      | isInBounds heights pos && not (visited!pos) && (heights!pos) == 'S' = 'a' `chardiff` (heights!(x,y)) <= 1
      | isInBounds heights pos && not (visited!pos) = if (heights!pos) /= 'E' then (heights!pos) `chardiff` (heights!(x,y)) <= 1 else 'z' `chardiff` (heights!(x,y)) <= 1
      | otherwise = False

isInBounds :: Heightmap -> Coord -> Bool
isInBounds heights (x,y) = x >= xlow && x <= xhigh && y >= ylow && y <= yhigh
  where
    ((xlow, ylow), (xhigh, yhigh)) = bounds heights

findInArr :: Heightmap -> Char -> Coord
findInArr height c = fst . head . filter ((==c) . snd) . assocs $ height

findAllInArr :: Heightmap -> Char -> [Coord]
findAllInArr height c = map fst . filter ((==c) . snd) . assocs $ height

toArray :: String -> Heightmap
toArray s = listArray ((0,0),(bound_x,bound_y)) . filter (not . isSpace) $ s
  where
    bound_x = length ls - 1
    bound_y = length(head ls) - 1
    ls = lines s

showArray :: Show t => Array (Int,Int) t -> String
showArray arr = concatMap (showLine arr) [0..bound_x]
  where
    showLine arr lineIndex = concatMap (showEntry arr lineIndex) [0..bound_y] ++ "\n"
    showEntry arr lineIndex colIndex = show (arr!(lineIndex, colIndex)) ++ " "
    (bound_x, bound_y)= snd . bounds $ arr