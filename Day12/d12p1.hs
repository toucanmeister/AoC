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
type Path = [Coord]

solveProblem :: String -> String
solveProblem s = show $ shortestPath heights emptyVisitedMap start
  where
    emptyVisitedMap = array (bounds heights) [((i,j), False) | i <- [xlow..xhigh], j <- [ylow..yhigh]] -- Array to keep track of where we've visited
    ((xlow, ylow), (xhigh, yhigh)) = bounds heights
    start = findStart heights
    heights = toArray s

chardiff :: Char -> Char -> Int
chardiff a b = ord a - ord b

shortestPath :: Heightmap -> Visitedmap -> Coord -> (Path, Int)
shortestPath heights visited pos 
  | heights!pos == 'E' = ([], 0)
  | null $ possibleDirections heights visited pos = ([], 1000000)
  | otherwise = (pos:minpath, min_n+1)
    where
      (minpath, min_n) = minimumBy comparepathlengths paths
      comparepathlengths x y = compare (snd x) (snd y)
      paths = map (shortestPath heights updatedVisits) $ possibleDirections heights visited pos
      updatedVisits = trace (showArray visited) visited//[(position,True) | position <- possibleDirections heights visited pos]

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