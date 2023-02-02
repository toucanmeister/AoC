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
type Sensor = Coord
type Beacon = Coord

solveProblem :: String -> String
solveProblem s = show . length . filter (`isInRadiusOfAny` sensorsWithRadius) $ rowNoBeacons
  where
    rowNoBeacons = filter (`notElem` map snd sensors) row
    row = [(x,2000000) | x <- [xmin..xmax]]
    (xmin, xmax) = xBounds sensorsWithRadius
    sensorsWithRadius = map radius sensors
    sensors = rights . map (myparse lineP) . lines $ s

xBounds :: [(Sensor,Int)] -> (Int,Int)
xBounds sensorpairs = (lowerbound, upperbound)
  where
    lowerbound = minimum $ map (\x -> fst ( fst x) - snd x) sensorpairs
    upperbound = maximum $ map (\x -> fst ( fst x) + snd x) sensorpairs

isInRadiusOfAny :: Coord -> [(Sensor, Int)] -> Bool
isInRadiusOfAny pos = any (pos `isInRadiusOf`)

isInRadiusOf :: Coord -> (Sensor, Int) -> Bool
isInRadiusOf pos (sensor,radius) = dist pos sensor <= radius

radius :: (Sensor, Beacon) -> (Sensor, Int)
radius (sensor, beacon) = (sensor, dist sensor beacon)

dist :: Coord -> Coord -> Int
dist (x1,y1) (x2,y2) = abs(x1-x2) + abs(y1-y2)

lineP :: P.GenParser Char st (Sensor, Beacon)
lineP = do
  P.string "Sensor at x="
  sensor_x <- intP
  P.string ", y="
  sensor_y <- intP
  P.string ": closest beacon is at x="
  beacon_x <- intP
  P.string ", y="
  beacon_y <- intP
  return ((sensor_x,sensor_y), (beacon_x,beacon_y))

intP :: P.GenParser Char st Int
intP = do
  minus <- P.optionMaybe (P.char '-')
  s <- P.many1 P.digit
  let num = read @Int s
  return $ case minus of Just '-' -> -num; Nothing -> num