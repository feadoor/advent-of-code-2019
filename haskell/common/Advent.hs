module Advent (
    rawInput, inputLines, parsedInput, parsedLines, solve, PlainString(..),
    Parser, parseInt, parseLines
) where

import Data.Void
import System.Environment
import Text.Megaparsec (parse, Parsec, sepBy)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Printf

-- Parsing

type Parser = Parsec Void String

parseInt :: Integral a => Parser a
parseInt = signed (return ()) decimal

parseLines :: Parser a -> Parser [a]
parseLines p = p `sepBy` eol

-- Input grabbing

day :: IO Int
day = (read . take 2 . drop 3) <$> getProgName

rawInput :: IO String
rawInput = do
    args <- getArgs
    case args of
        []     -> day >>= readFile . printf "../data/%02d.txt"
        file:_ -> readFile file

inputLines :: IO [String]
inputLines = lines <$> rawInput

parseS :: Monad m => Parser a -> String -> m a
parseS p = either (fail . errorBundlePretty) return . parse p "input"

parsedInput :: Parser a -> IO a
parsedInput p = rawInput >>= parseS p

parsedLines :: Parser a -> IO [a]
parsedLines p = inputLines >>= traverse (parseS p)

-- Solution running

newtype PlainString = PlainString String
instance Show PlainString where show (PlainString s) = s

solve :: (Show s, Show t) => IO a -> (a -> s) -> (a -> t) -> IO ()
solve readi p1 p2 = do
    input <- readi
    print $ p1 input
    print $ p2 input
