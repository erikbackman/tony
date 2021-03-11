module Main where

import Text.Parsec
import Text.Parsec.String
import Data.Char
import Data.Either
import Data.List

main :: IO ()
main = interact $ formatCsv . parseInput 
  where
    formatCsv  = unlines . fmap (tnToCsv ";")
    parseInput = rights . fmap parse' . lines
    parse'     = parse tn ""

data TN = TN
  { _prefix :: String
  , _value :: String
  } deriving Show

prefix :: Parser String
prefix = do
  c <- char 'T'
  n <- many1 (satisfy isNumber)
  pure (c:n)

value :: Parser String
value = many1 (satisfy (\c -> c `elem` "!$%&|*+-/:<=>?@^_~#." || isAlphaNum c || isSpace c))

betweenParens :: Parser a -> Parser a
betweenParens = between (char '(') (char ')')

tn :: Parser TN
tn = do
  manyTill anyChar space
  spaces
  pre <- prefix 
  optional space
  val <- betweenParens value 
  pure $ TN pre val

tnToCsv :: String -> TN -> String
tnToCsv delim (TN pre val) = pre <> delim <> val
