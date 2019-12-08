{-# Language RecordWildCards #-}

import Advent

import Control.Applicative
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Text.Megaparsec (sepBy, single)

-- Memory definitions

type Memory = Seq Int

(!) :: Memory -> Int -> Int
(!) = Seq.index

-- VM definitions

data Vm = Vm { pc :: Int, mem :: Memory }

arg :: Int -> Vm -> Int
arg i v@Vm { .. } = mem ! (pc + i)

val :: Int -> Vm -> Int
val i v@Vm { .. } = mem ! (arg i v)

set :: Int -> Int -> Vm -> Vm
set i x v@Vm { .. } = v { mem = Seq.update i x mem }

tick :: Int -> Vm -> Vm
tick n v@Vm { .. } = v { pc = pc + n }

op1 :: Vm -> Vm
op1 v@Vm { .. } = tick 4 . set (arg 3 v) (val 1 v + val 2 v) $ v

op2 :: Vm -> Vm
op2 v@Vm { .. } = tick 4 . set (arg 3 v) (val 1 v * val 2 v) $ v

run :: Vm -> Vm
run v = case arg 0 v of
    1  -> run $ op1 v
    2  -> run $ op2 v
    99 -> v

-- Inputs and outputs to a VM

setup :: Int -> Int -> Memory -> Vm
setup a b m = let vm = Vm { pc = 0, mem = m }
              in set 1 a . set 2 b $ vm

output :: Int -> Int -> Memory -> Int
output a b m = let Vm { mem = mem } = run $ setup a b m
               in mem ! 0

find :: Int -> Memory -> (Int, Int)
find x m = let output' m (a, b) = output a b m
           in head . filter ((== x) . output' m) $ liftA2 (,) [0..99] [0..99]

-- Input parsing

parseMem :: Parser Memory
parseMem = Seq.fromList <$> parseInt `sepBy` single ','

-- Putting it all together

part1 :: Memory -> Int
part1 = output 12 2

part2 :: Memory -> Int
part2 = nv . find 19690720
        where nv (a, b) = 100 * a + b

readInput :: IO Memory
readInput = parsedInput parseMem

main = solve readInput part1 part2
