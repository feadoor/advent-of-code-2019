import Advent

import Data.List
import Text.Megaparsec (single)

-- Utilities

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

-- Password conditions

increasing :: Ord a => [a] -> Bool
increasing = all ((<=) <$> fst <*> snd) . pairs

hasDuplicate :: Eq a => [a] -> Bool
hasDuplicate = any (>= 2) . map length . groupBy (==)

hasExactDuplicate :: Eq a => [a] -> Bool
hasExactDuplicate = any (== 2) . map length . groupBy (==)

-- Input parsing

parseRange :: Parser (Int, Int)
parseRange = (,) <$> parseInt <* single '-' <*> parseInt

-- Putting it all together

part1 :: (Int, Int) -> Int
part1 (x, y) = length . filter ((&&) <$> increasing <*> hasDuplicate) $ map show [x..y]

part2 :: (Int, Int) -> Int
part2 (x, y) = length . filter ((&&) <$> increasing <*> hasExactDuplicate) $ map show [x..y]

readInput :: IO (Int, Int)
readInput = parsedInput parseRange

main = solve readInput part1 part2
