import System.IO
import Data.List
import Data.List.Split
import Data.Char
import Debug.Trace ( traceShow , trace )
import Data.Either
import Data.Maybe
import Data.Array
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Text.Parsec.Char as P

mytrace x = traceShow x x

main = do
  handle <- openFile "testinput.txt" ReadMode
  contents <- hGetContents handle
  putStrLn . solveProblem $ contents
  hClose handle

data Packet = L Int | R [Packet]

solveProblem :: String -> String
solveProblem = id

listP :: P.GenParser Char st Packet
listP = do
  P.char '['
  content <- P.sepBy (P.choice [intP, listP]) (P.char ',')
  P.char ']'
  return content

intP :: P.GenParser Char st Int
intP = do
  s <- P.many1 P.digit
  return [read @Int s]
