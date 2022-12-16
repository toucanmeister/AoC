import System.IO
import Data.List
import Data.List.Split
import Debug.Trace ( traceShow , trace )
import Data.Either
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import Data.Maybe
import Data.Array

mytrace x = traceShow x x

myparse p = P.parse p "" 

main:: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  putStrLn . solveProblem $ contents
  hClose handle

type Coord = (Int,Int)
type RockPath = [Coord]
data Thing = Air | Rock | Sand | Source
  deriving Eq
type Area = Array Coord Thing

instance Show Thing
  where
    show :: Thing -> String
    show Air = "."
    show Rock = "#"
    show Sand = "o"
    show Source = "+"

solveProblem :: String -> String
solveProblem s = show . length . takeUntilFirstRepeat $ areasInTime
  where
    areasInTime = tail $ iterate dropASand areaWithFloor
    areaWithFloor = area // [((i,ymax),Rock) | i <- [xmin..xmax]]
    ((xmin,ymin),(xmax,ymax)) = area_bounds
    areaWithSource = area // [((500,0),Source)]
    area = placeRocks (emptyArea area_bounds) rockpaths
    area_bounds = getBounds rockpaths
    rockpaths = rights . map (myparse rocksP) . lines $ s

takeUntilFirstRepeat :: [Area] -> [Area]
takeUntilFirstRepeat (a1:a2:as)
  | a1 == a2 = [a1]
  | otherwise = a1 : takeUntilFirstRepeat (a2:as)

dropASand :: Area -> Area
dropASand area = dropSand areaWithSand (500,0)
  where areaWithSand = area//[((500,0),Sand)]

dropSand :: Area -> Coord -> Area
dropSand area (x,y)
  | area!(x,y+1) == Air   = dropSand (area//[((x,y),Air), ((x,y+1),Sand)]) (x,y+1)
  | area!(x-1,y+1) == Air = dropSand (area//[((x,y),Air), ((x-1,y+1),Sand)]) (x-1,y+1)
  | area!(x+1,y+1) == Air = dropSand (area//[((x,y),Air), ((x+1,y+1),Sand)]) (x+1,y+1)
  | otherwise = area

isOutOfBoundsOf :: Coord -> Area -> Bool
isOutOfBoundsOf pos area = not $ isInBoundsOf pos area

isInBoundsOf :: Coord -> Area -> Bool
isInBoundsOf (x,y) area = x >= xlow && x <= xhigh && y >= ylow && y <= yhigh
  where
    ((xlow, ylow), (xhigh, yhigh)) = bounds area

placeRocks :: Area -> [RockPath] -> Area
placeRocks = foldr placeOnePath
  where
    placeOnePath (edge1:edge2:path) area = placeOnePath (edge2:path) area // [((i,j),Rock) | i <- allBetween (fst edge1) (fst edge2),  j <- allBetween (snd edge1) (snd edge2)]
    placeOnePath (edge:path) area = area
    placeOnePath [] area = area

allBetween :: Int -> Int -> [Int]
allBetween a b
  | a <= b = [a..b]
  | a >  b = [b..a]

emptyArea :: (Coord, Coord) -> Area
emptyArea area_bounds@((xmin,ymin),(xmax,ymax)) = array area_bounds [((i,j),Air) | i <- [xmin..xmax], j <- [ymin..ymax]]

getBounds :: [RockPath] -> (Coord, Coord)
getBounds paths = ((minimum xs - 5*x_span, y_min), (maximum xs + 5*x_span, maximum ys +2))
  where
    x_span = maximum xs - minimum xs
    y_min = if minimum ys > 0 then 0 else minimum ys
    ys = map snd rocks
    xs = map fst rocks
    rocks = concat paths

rocksP :: P.GenParser Char st RockPath
rocksP = P.sepBy coordP (P.string " -> ")

coordP :: P.GenParser Char st Coord
coordP = do
  x <- intP
  P.char ','
  y <- intP
  return (x,y)

intP :: P.GenParser Char st Int
intP = do
  s <- P.many1 P.digit
  return $ read @Int s

showArray :: Show t => Array (Int,Int) t -> String
showArray arr = concatMap (showLine arr) [ymin..ymax]
  where
    showLine arr x_index = concatMap (showEntry arr x_index) [xmin..xmax] ++ "\n"
    showEntry arr x_index y_index = show (arr!(y_index, x_index)) ++ " "
    ((xmin, ymin),(xmax, ymax)) = bounds arr