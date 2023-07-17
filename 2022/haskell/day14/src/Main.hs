module Main where

import Data.List (sort)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

type Point = (Int, Int)

-- A point is a comma separated pair of integers etc: 123,192
parsePoint :: Parser Point
parsePoint = do
  x <- L.decimal
  char ','
  y <- L.decimal
  return (x, y)

parseArrow :: Parser ()
parseArrow = do
  char '-'
  char '>'
  return ()

parsePath :: Parser [Point]
parsePath = parsePoint `sepBy` parseArrow

parseInput :: Parser [[Point]]
parseInput = parsePath `sepBy` newline

main = do
  input <- readFile "../../inputs/14"
  -- filter out all whitespaces and remove the trailing newline
  let text = T.pack $ filter (/= ' ') input

  let parsed = parse parseInput "" text

  -- either print the error or run the program
  case parsed of
    Left err -> putStr (errorBundlePretty err)
    Right val -> do print $ allPoints val
