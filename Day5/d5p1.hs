import System.IO
import Data.List.Split
import Data.List
import Data.Char
import Data.Foldable
import Debug.Trace
import Control.Applicative

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print . solveProblem $ contents
  hClose handle

type Stack t = [t]
type Crate = Char

solveProblem :: String -> String
solveProblem s = 
  let
    [initpart, instructionpart] = splitOn "\n\n" s
    stacks = parseStacks initpart
    instructions = parseInstructions instructionpart
  in doInstructions stacks instructions

-- My Parser
newtype Parser a = Parser { parse :: String -> Maybe(a,String)}

charP :: Char -> Parser Char
charP c = Parser parse_c
  where
    parse_c [] = Nothing
    parse_c (x:s)
      | x == c = Just (c,s)
      | x /= c = Nothing

stringP :: String -> Parser String
stringP = mapM charP

spaceP :: Parser Char
spaceP = charP ' ' <|> charP '\n' <|> charP '\t' <|> charP '\r'

skipSpace :: Parser String
skipSpace = many spaceP

alphaP :: Parser Char
alphaP = asum [charP c | c <- ['A'..'Z']++['a'..'z']]

crateP :: Parser Crate
crateP = do
  _ <- charP '['
  c <- alphaP
  _ <- charP ']'
  return c

-- Type class Instances
instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> do
    (x,s') <- p s
    return (f x, s')

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x,s)
  Parser f <*> Parser p = Parser $ \s -> do
    (f', s1) <- f s
    (x, s2) <- p s1
    return (f' x, s2)

instance Monad Parser where
  Parser p >>= f = Parser $ \s -> do
    (x, s1) <- p s
    parse (f x) s1

instance MonadFail Parser where
  fail _ = Parser $ const Nothing

instance Alternative Parser where
  empty = fail ""
  Parser p <|> Parser q = Parser $ \s ->
    case p s of
      Nothing -> q s
      Just x  -> Just x