import Advent
import Advent.Intcode

import Data.Function
import Data.List

-- Amplifier sequencing

singleAmp :: Memory -> Int -> [Int] -> [Int]
singleAmp mem phase inputs = fst . run (phase : inputs) $ vm mem

sequential :: Memory -> [Int] -> [Int] -> [Int]
sequential mem = foldl (flip (.)) id . map (singleAmp mem)

looped :: Memory -> [Int] -> Int -> [Int]
looped mem phases initial = fix (sequential mem phases . (initial :))

-- Putting it all together

part1 :: Memory -> Int
part1 m = maximum . map (head . ($ [0]) . sequential m) $ permutations [0..4]

part2 :: Memory -> Int
part2 m = maximum . map (last . ($ 0) . looped m) $ permutations [5..9]

readInput :: IO Memory
readInput = parsedInput parseMem

main = solve readInput part1 part2
