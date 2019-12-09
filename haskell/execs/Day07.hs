import Advent
import Advent.Intcode

import Data.List

-- Amplifier sequencing

linkedAmps :: ([[Int]] -> [[Int]]) -> Memory -> [Int] -> [[Int]]
linkedAmps link m ps = outputs
    where runningVms = zipWith run inputs vms
          vms = map (vm . const m) ps
          inputs = zipWith (:) ps (link outputs)
          outputs = map fst runningVms

sequentialAmps :: Memory -> [Int] -> [[Int]]
sequentialAmps = linkedAmps ([0] : )

loopedAmps :: Memory -> [Int] -> [[Int]]
loopedAmps = linkedAmps $ (:) <$> ((0 : ) . last) <*> id

-- Putting it all together

part1 :: Memory -> Int
part1 m = maximum . map (last . last . sequentialAmps m) $ permutations [0..4]

part2 :: Memory -> Int
part2 m = maximum . map (last . last . loopedAmps m) $ permutations [5..9]

readInput :: IO Memory
readInput = parsedInput parseMem

main = solve readInput part1 part2
