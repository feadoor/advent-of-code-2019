import Advent
import Advent.Intcode

-- Putting it all together

part1 :: Memory -> Int
part1 = last . ($ [1]) . runner . vm

part2 :: Memory -> Int
part2 = last . ($ [5]) . runner . vm

readInput :: IO Memory
readInput = parsedInput parseMem

main = solve readInput part1 part2
