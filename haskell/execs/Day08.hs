import Advent

import Data.Char
import Data.List
import Data.Ord
import Text.Megaparsec (many)
import Text.Megaparsec.Char (digitChar)

-- Layer manipulation

rows = 25
cols = 6

chunks :: Int -> [Int] -> [[Int]]
chunks _ [] = []
chunks n xs = take n xs : (chunks n $ drop n xs)

layers :: [Int] -> [[Int]]
layers = chunks $ rows * cols

count :: Int -> [Int] -> Int
count n = length . filter (== n)

squash :: [[Int]] -> [Int]
squash = map (head . filter (/= 2)) . transpose

arrange :: [Int] -> [[Int]]
arrange = chunks rows

-- Input parsing

parsePixels :: Parser [Int]
parsePixels = (map digitToInt) <$> many digitChar

parseLayers :: Parser [[Int]]
parseLayers = layers <$> parsePixels

-- Putting it all together

part1 :: [[Int]] -> Int
part1 = ((*) <$> count 1 <*> count 2) . minimumBy (comparing $ count 0)

part2 :: [[Int]] -> String
part2 = unlines . map (map charFor) . arrange . squash
    where charFor x = if x == 0 then ' ' else 'X'

readInput :: IO [[Int]]
readInput = parsedInput parseLayers

main = solve readInput part1 (PlainString . part2)
