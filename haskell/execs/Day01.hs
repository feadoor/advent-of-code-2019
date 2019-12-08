import Advent

-- Fuel calculations

fuel :: Int -> Int
fuel = subtract 2 . (`div` 3)

fuel' :: Int -> Int
fuel' = sum . drop 1 . takeWhile (>0) . iterate fuel

-- Putting it all together

part1 ::  [Int] -> Int
part1 = sum . map fuel

part2 :: [Int] -> Int
part2 = sum . map fuel'

readInput :: IO [Int]
readInput = parsedLines parseInt

main = solve readInput part1 part2
