module Main where

import Text.Parsec hiding (Line)
import Text.Parsec.String
import Data.Char
import Data.Either
import Data.List

main :: IO ()
main = interact $ fileToCsv ";" . linesToFile . parseInput 
  where
    -- formatCsv  = unlines . fmap (fileToCsv ";")
    parseInput = rights . fmap parse' . lines
    parse'     = parse line ""

data XN   = XN String deriving Show
data TN   = TN { _prefix :: String, _value :: String } deriving Show
data Line = XNLine XN | TNLine TN deriving Show
data File = File { name :: String, values :: [TN] } deriving Show

prefix :: Parser String
prefix = do
  c <- char 'T'
  n <- many1 (satisfy isNumber)
  pure (c:n)

value :: Parser String
value = many1 (satisfy (\c -> c `elem` "!$%&|*+-/:<=>?@^_~#." || isAlphaNum c || isSpace c))

betweenParens :: Parser a -> Parser a
betweenParens = between (char '(') (char ')')

line :: Parser Line
line = try xn <|> tn

linesToFile :: [Line] -> File
linesToFile = foldl' (\z x -> case x of
                               XNLine (XN xn) -> z { name = xn }
                               TNLine tn      -> z { values = tn : values z })
                    (File "" []) 

fileToCsv :: String -> File -> String
fileToCsv delim (File n vs) = unlines $ fmap toCsv vs
  where
    toCsv (TN pre val) = n <> delim <> pre <> delim <> val

tn :: Parser Line
tn = do
  manyTill anyChar space
  spaces
  pre <- prefix 
  optional space
  val <- betweenParens value 
  pure $ TNLine (TN pre val)

xn :: Parser Line
xn = do
  manyTill anyChar (char '(')
  pre <- string "XN"
  val <- manyTill anyChar (char ')') 
  pure $ XNLine (XN (pre <> val))
