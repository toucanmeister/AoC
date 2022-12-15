import System.IO
import Data.List
import Data.List.Split
import Debug.Trace ( traceShow , trace )
import Data.Either
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import Data.Maybe

mytrace x = traceShow x x

myparse p = P.parse p "" 

main:: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  putStrLn . solveProblem $ contents
  hClose handle

data Packet = L Int | R [Packet]
  deriving (Show, Eq)

instance Ord Packet where
  compare :: Packet -> Packet -> Ordering
  compare (L a) (L b)
    | a < b  = LT
    | a > b  = GT
    | a == b = EQ
  compare (R (x:xs)) (R (y:ys))
    | x < y     = LT
    | x > y     = GT
    | otherwise = compare (R xs) (R ys)
  compare (R []) (R (_:_)) = LT
  compare (R (_:_)) (R []) = GT
  compare (R []) (R [])    = EQ
  compare (L a) (R lst) = compare (R [L a]) (R lst)
  compare (R lst) (L b) = compare (R lst) (R [L b])

solveProblem :: String -> String
solveProblem s = show $ divider1_index * divider2_index
  where
    divider1_index = (1+) $ fromJust $ divider1 `elemIndex` sortedpackets
    divider2_index = (1+) $ fromJust $ divider2 `elemIndex` sortedpackets
    sortedpackets = sort $ divider1 : divider2 : packets
    divider1 = R [R [L 2]]
    divider2 = R [R [L 6]]
    packets = concatMap (rights . map (myparse packetP)) packetpairstrings
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
