import Advent
import Advent.Intcode

import Data.Function
import Data.List

-- Amplifier sequencing

singleAmp :: Memory -> Int -> [Int] -> [Int]
singleAmp mem phase inputs = fst . run (phase : inputs) $ vm mem

sequential :: [[Int] -> [Int]] -> [Int] -> [Int]
sequential = foldl (flip (.)) id

looped :: [[Int] -> [Int]] -> Int -> [Int]
looped amps initial = fix (\inputs -> sequential amps (initial : inputs))

-- Putting it all together

part1 :: Memory -> Int
part1 m = maximum . map (head . ($ [0]) . sequential . map (singleAmp m)) $ permutations [0..4]

part2 :: Memory -> Int
part2 m = maximum . map (last . ($ 0) . looped . map (singleAmp m)) $ permutations [5..9]

readInput :: IO Memory
readInput = parsedInput parseMem

main = solve readInput part1 part2
