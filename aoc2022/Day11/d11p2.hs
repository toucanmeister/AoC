import System.IO
import Data.List
import Data.List.Split
import Data.Char
import Debug.Trace ( traceShow , trace )
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Text.Parsec.Char as P
import qualified Text.Parsec.Combinator as P
import Data.Either
import Data.Maybe

mytrace x = traceShow x x

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  putStrLn . solveProblem $ contents
  hClose handle

solveProblem :: String -> String
solveProblem s = show . product .  take 2 . reverse . sort . map countInspects . rounds 10000 $ monkeys
  where monkeys = fromRight [] $ myparse (P.sepBy monkeyP P.spaces) s

-- Types
type Worry = Integer
data Monkey = Monkey {
  monkeyId :: Int,
  items :: [Worry],
  op :: Worry -> Worry,
  throwTo :: Worry -> Int,
  countInspects :: Int
}
instance Show Monkey where
  show :: Monkey -> String
  show m = "Monkey " ++ show (monkeyId m) ++ ":\n" 
        ++ "|" ++ show (items m) ++ "\n"
        ++ "|op 1 = " ++ show (op m 1) ++ "\n" 
        ++ "|throwto 12 = " ++ show (throwTo m 12) ++ "\n"

-- Rounds and Turns
rounds :: Int -> [Monkey] -> [Monkey]
rounds n ms = iterate oneRound ms !! n

oneRound :: [Monkey] -> [Monkey]
oneRound ms = foldl' oneTurn ms [0..(length ms - 1)]

oneTurn :: [Monkey] -> Int -> [Monkey]
oneTurn ms currentm = foldl' (inspectAndThrow currentm) ms (items (ms!!currentm))

replace :: Show t => [t] -> Int -> t -> [t] --replaces element at an index with given element
replace l i e = pre ++ e:post
  where 
    post = tail postwith
    (pre, postwith) = splitAt i l

inspectAndThrow :: Int -> [Monkey] -> Worry -> [Monkey]
inspectAndThrow currentm ms _ = throw currentm inspectedms
  where inspectedms = inspect currentm ms

inspect :: Int -> [Monkey] -> [Monkey]
inspect currentm ms = replace ms currentm newmonkey
  where
    newmonkey = Monkey{monkeyId=_id, items=newitems, op=_op, throwTo=_throwTo, countInspects=_countInspects+1}
    newitems = newitem : tail _items
    newitem = _op (head _items) `mod` modulo
    Monkey{monkeyId=_id, items=_items, op=_op, throwTo=_throwTo, countInspects=_countInspects} = ms!!currentm
    modulo = 5*7*13*11*3*2*17*19

removeFirstItem :: Monkey -> Monkey
removeFirstItem Monkey{monkeyId=_id, items=_items, op=_op, throwTo=_throwTo, countInspects=_countInspects} = Monkey{monkeyId=_id, items= tail _items, op=_op, throwTo=_throwTo, countInspects=_countInspects}

throw :: Int -> [Monkey] -> [Monkey]
throw currentm ms = replace monkeys_with_new_send receivemonkey_id new_receivemonkey
  where
    monkeys_with_new_send = replace ms currentm new_sendmonkey
    new_receivemonkey = Monkey{monkeyId=r_id, items = r_items++[item], op=r_op, throwTo=r_throwTo, countInspects=r_countInspects}
    new_sendmonkey = removeFirstItem sendmonkey
    receivemonkey@Monkey{monkeyId=r_id, items=r_items, op=r_op, throwTo=r_throwTo, countInspects=r_countInspects} = ms !! receivemonkey_id
    receivemonkey_id = s_throwTo item
    item = head s_items
    sendmonkey@Monkey{monkeyId=s_id, items=s_items, op=s_op, throwTo=s_throwTo} = ms !! currentm

-- Parsing
myparse x = P.parse x ""

monkeyP :: P.GenParser Char st Monkey
monkeyP = do
  _id <- idP
  _items <- startingItemsP
  _op <- operationP
  test <- testP
  truecase <- trueCaseP
  falsecase <- falseCaseP
  return Monkey{monkeyId=_id, items=_items, op=_op, throwTo = \x -> if test x then truecase else falsecase, countInspects = 0}

intP :: P.GenParser Char st Int
intP = do
  s <- P.many1 P.digit
  return (read @Int s)

idP :: P.GenParser Char st Int
idP = do
  P.string "Monkey "
  id <- intP
  P.char ':'
  return id

startingItemsP :: P.GenParser Char st [Integer]
startingItemsP = do
  P.spaces
  P.string "Starting items:"
  P.spaces
  map toInteger <$> P.sepBy intP (P.string ", ")

operationP :: P.GenParser Char st (Worry -> Worry)
operationP = do
  P.spaces
  P.string "Operation: new = old "
  operator <- P.char '+' P.<|> P.char '*'
  P.spaces
  num <- P.optionMaybe intP
  if isNothing num
    then P.string "old"
    else P.string ""
  return (buildOperation operator num)

buildOperation :: Char -> Maybe Int -> Worry -> Worry
buildOperation '+' (Just num) = (+ toInteger num)
buildOperation '*' (Just num) = (* toInteger num)
buildOperation '+' Nothing = \x -> x+x
buildOperation '*' Nothing = \x -> x*x

testP :: P.GenParser Char st (Worry -> Bool)
testP = do
  P.spaces
  P.string "Test: divisible by "
  num <- intP
  return (\x -> x `mod` toInteger num == 0)

trueCaseP :: P.GenParser Char st Int
trueCaseP = do
  P.spaces
  P.string "If true: throw to monkey "
  intP

falseCaseP :: P.GenParser Char st Int
falseCaseP = do
  P.spaces
  P.string "If false: throw to monkey "
  intP