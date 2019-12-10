import Advent
import Advent.Intcode

import Data.Function
import Data.List

-- Amplifier sequencing

amps :: Memory -> [Int] -> [[Int] -> [Int]]
amps = map . amp where amp mem phase inputs = run (phase : inputs) $ vm mem

sequential :: [[Int] -> [Int]] -> [Int] -> [Int]
sequential = foldr (.) id

looped :: [[Int] -> [Int]] -> Int -> [Int]
looped fs initial = fix (sequential fs . (initial : ))

-- Putting it all together

part1 :: Memory -> Int
part1 m = maximum . map (head . ($ [0]) . sequential . amps m) $ permutations [0..4]

part2 :: Memory -> Int
part2 m = maximum . map (last . ($ 0) . looped . amps m) $ permutations [5..9]

readInput :: IO Memory
readInput = parsedInput parseMem

main = solve readInput part1 part2
