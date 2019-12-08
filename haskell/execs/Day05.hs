{-# Language RecordWildCards #-}

import Advent
import Advent.Intcode

import Control.Monad.Writer

-- Putting it all together

part1 :: Memory -> Int
part1 = last . snd . runWriter  . run [1] . vm

part2 :: Memory -> Int
part2 = last . snd . runWriter  . run [5] . vm

readInput :: IO Memory
readInput = parsedInput parseMem

main = solve readInput part1 part2
