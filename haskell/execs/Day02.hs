import Advent
import Advent.Intcode

import Control.Applicative
import Data.Sequence (Seq, index)
import qualified Data.Sequence as Seq

-- Inputs and outputs to a VM

setup :: Int -> Int -> Memory -> Memory
setup a b = Seq.update 1 a . Seq.update 2 b

finalState :: Vm -> Vm
finalState vm = case step vm of
    Step vm' -> finalState vm'
    StepHalt -> vm

output :: Int -> Int -> Memory -> Int
output a b m = let Vm { mem = mem } = finalState . vm . setup a b $ m
               in mem `index` 0

find :: Int -> Memory -> (Int, Int)
find x m = let output' m (a, b) = output a b m
           in head . filter ((== x) . output' m) $ liftA2 (,) [0..99] [0..99]

-- Putting it all together

part1 :: Memory -> Int
part1 = output 12 2

part2 :: Memory -> Int
part2 = nv . find 19690720
        where nv (a, b) = 100 * a + b

readInput :: IO Memory
readInput = parsedInput parseMem

main = solve readInput part1 part2
