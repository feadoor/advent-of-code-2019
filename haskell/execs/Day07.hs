import Advent
import Advent.Intcode

import Control.Monad.Writer
import Data.List
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- Amplifier sequencing

sequentialAmps :: Memory -> [Int] -> [[Int]]
sequentialAmps m ps = outputs
    where runningVms = zipWith run inputs vms
          vms = map (vm . const m) ps
          inputs = zipWith (:) ps ([0] : outputs)
          outputs = map (snd . runWriter) runningVms

loopedAmps :: Memory -> [Int] -> [[Int]]
loopedAmps m ps = outputs
    where runningVms = zipWith run inputs vms
          vms = map (vm . const m) ps
          inputs = zipWith (:) ps (([0] ++ last outputs) : outputs)
          outputs = map (snd . runWriter) runningVms

-- Putting it all together

part1 :: Memory -> Int
part1 m = maximum . map (last . last . sequentialAmps m) $ permutations [0..4]

part2 :: Memory -> Int
part2 m = maximum . map (last . last . loopedAmps m) $ permutations [5..9]

readInput :: IO Memory
readInput = parsedInput parseMem

main = solve readInput part1 part2
