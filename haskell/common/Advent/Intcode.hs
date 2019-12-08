{-# Language RecordWildCards #-}

module Advent.Intcode (Memory, Vm(..), parseMem, (!), vm, run) where

import Advent (Parser, parseInt)

import Control.Monad.Writer
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Text.Megaparsec (sepBy, single)

-- Memory definitions

type Memory = Seq Int

(!) :: Memory -> Int -> Int
(!) = Seq.index

parseMem :: Parser Memory
parseMem = Seq.fromList <$> parseInt `sepBy` single ','

-- General VM definitions

data Vm = Vm { pc :: Int, mem :: Memory }

vm :: Memory -> Vm
vm m = Vm { pc = 0, mem = m }

arg :: Int -> Vm -> Int
arg i v@Vm { .. } = mem ! (pc + i)

val :: Int -> Vm -> Int
val i v@Vm { .. } = mem ! (arg i v)

load :: Int -> Vm -> Int
load i v@Vm { .. } = case mode i v of
    0 -> val i v
    1 -> arg i v
    x -> error $ "Unknown parameter mode " ++ show x ++ " at " ++ show pc

opcode :: Vm -> Int
opcode v@Vm { .. } = (arg 0 v) `mod` 100

mode :: Int -> Vm -> Int
mode i v = arg 0 v `div` (10 ^ (i + 1)) `mod` 10

set :: Int -> Int -> Vm -> Vm
set x i v@Vm { .. } = v { mem = Seq.update i x mem }

tick :: Int -> Vm -> Vm
tick n v@Vm { .. } = v { pc = pc + n }

jump :: Int -> Vm -> Vm
jump n v@Vm { .. } = v { pc = n }

-- Program execution definitions

op1 :: Vm -> Vm
op1 v = tick 4 . set (load 1 v + load 2 v) (arg 3 v) $ v

op2 :: Vm -> Vm
op2 v = tick 4 . set (load 1 v * load 2 v) (arg 3 v) $ v

op3 :: Int -> Vm -> Vm
op3 input v = tick 2 . set input (arg 1 v) $ v

op4 :: Vm -> Writer [Int] Vm
op4 v = writer (tick 2 v, [load 1 v])

op5 :: Vm -> Vm
op5 v = let cond = load 1 v /= 0
            next = if cond then jump (load 2 v) else tick 3
        in  next v

op6 :: Vm -> Vm
op6 v = let cond = load 1 v == 0
            next = if cond then jump (load 2 v) else tick 3
        in  next v

op7 :: Vm -> Vm
op7 v = let n = if load 1 v < load 2 v then 1 else 0
        in tick 4 . set n (arg 3 v) $ v

op8 :: Vm -> Vm
op8 v = let n = if load 1 v == load 2 v then 1 else 0
        in tick 4 . set n (arg 3 v) $ v

run :: [Int] -> Vm -> Writer [Int] Vm
run inputs v@Vm { .. } = case opcode v of
    1  -> run inputs $ op1 v
    2  -> run inputs $ op2 v
    3  -> run (tail inputs) $ op3 (head inputs) v
    4  -> op4 v >>= run inputs
    5  -> run inputs $ op5 v
    6  -> run inputs $ op6 v
    7  -> run inputs $ op7 v
    8  -> run inputs $ op8 v
    99 -> return v
    x -> error $ "Unknown opcode " ++ show x ++ " at " ++ show pc