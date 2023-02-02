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

myparse p = P.parse p "" 

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  putStrLn . solveProblem $ contents
  hClose handle

data Packet = L Int | R [Packet]
  deriving (Show, Eq)

instance Ord Packet where
  compare (L a) (L b)
    | a < b = LT
    | a > b = GT
    | a== b = EQ
  compare (R (x:xs)) (R (y:ys))
    | x < y  = LT
    | x > y  = GT
    | x == y = compare (R xs) (R ys)
  compare (R []) (R (_:ys)) = LT
  compare (R (x:xs)) (R []) = GT
  compare (R []) (R [])     = EQ
  compare (L a) (R lst)  = compare (R [L a]) (R lst)
  compare (R lst) (L b)  = compare (R lst) (R [L b])

solveProblem :: String -> String
solveProblem s = show . sum . map (+1) . findIndices (uncurry (<=)) $ packetpairs
  where
    packetpairs = map (\x -> (head x, x!!1)) packetlists
    packetlists = map (rights . map (myparse packetP)) packetpairstrings
    packetpairstrings = map lines $ splitOn "\n\n" s

packetP :: P.GenParser Char st Packet
packetP = intP P.<|> listP

listP :: P.GenParser Char st Packet
listP = do
  P.char '['
  content <- P.sepBy packetP (P.char ',')
  P.char ']'
  return $ R content

intP :: P.GenParser Char st Packet
intP = do
  s <- P.many1 P.digit
  return $ L (read @Int s)
