import Advent
import Advent.Intcode

-- Putting it all together

part1 :: Memory -> Int
part1 = last . fst . run [1] . vm

part2 :: Memory -> Int
part2 = last . fst . run [5] . vm

readInput :: IO Memory
readInput = parsedInput parseMem

main = solve readInput part1 part2
