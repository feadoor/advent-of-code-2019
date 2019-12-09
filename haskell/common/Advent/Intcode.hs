{-# Language RecordWildCards #-}

module Advent.Intcode (Memory, Vm(..), parseMem, vm, run) where

import Advent (Parser, parseInt)

import Control.Monad.Writer
import Data.Sequence (Seq, (><), index)
import qualified Data.Sequence as Seq
import Data.Tuple (swap)
import Text.Megaparsec (sepBy, single)

-- Memory definitions

type Memory = Seq Int

(!>>) :: Memory -> Int -> Int
(!>>) m i = if length m <= i then 0 else m `index` i

(<<!) :: Memory -> Int -> Int -> Memory
(<<!) m i x = Seq.update i x newM
    where newM = if length m <= i then m >< Seq.replicate (i - length m + 1) 0 else m

parseMem :: Parser Memory
parseMem = Seq.fromList <$> parseInt `sepBy` single ','

-- Parameter definitions

data Param = Position Int | Immediate Int | Relative Int

-- General VM definitions

data Vm = Vm { pc :: Int, mem :: Memory, base :: Int }

vm :: Memory -> Vm
vm m = Vm { pc = 0, mem = m, base = 0 }

param :: Int -> Vm -> Param
param i v@Vm { .. } = let arg = mem !>> (pc + i) in case mode i v of
    0 -> Position arg
    1 -> Immediate arg
    2 -> Relative arg
    x -> error $ "Invalid parameter mode " ++ show x ++ " at " ++ show pc

get :: Param -> Vm -> Int
get p v@Vm { .. } = case p of
    Position arg  -> mem !>> arg
    Immediate arg -> arg
    Relative arg  -> mem !>> (base + arg)

set :: Int -> Param -> Vm -> Vm
set x p v@Vm { .. } = v { mem = (mem <<! i) x }
    where i = case p of
            Position arg -> arg
            Relative arg -> base + arg
            Immediate _  -> error $ "Attempt to write to immediate parameter at " ++ show pc

opcode :: Vm -> Int
opcode v@Vm { .. } = (mem !>> pc) `mod` 100

mode :: Int -> Vm -> Int
mode i v@Vm { .. } = (mem !>> pc) `div` (10 ^ (i + 1)) `mod` 10

tick :: Int -> Vm -> Vm
tick n v@Vm { .. } = v { pc = pc + n }

jump :: Int -> Vm -> Vm
jump n v@Vm { .. } = v { pc = n }

adjustBase :: Int -> Vm -> Vm
adjustBase n v@Vm { .. } = v { base = base + n }

-- Program execution definitions

op1 :: Vm -> Vm
op1 v = let [p1, p2, p3] = map (flip param v) [1..3]
        in tick 4 . set (get p1 v + get p2 v) p3 $ v

op2 :: Vm -> Vm
op2 v = let [p1, p2, p3] = map (flip param v) [1..3]
        in tick 4 . set (get p1 v * get p2 v) p3 $ v

op3 :: Int -> Vm -> Vm
op3 input v = let p1 = param 1 v in tick 2 . set input p1 $ v

op4 :: Vm -> (Vm, [Int])
op4 v = let p1 = param 1 v in (tick 2 v, [get p1 v])

op5 :: Vm -> Vm
op5 v = let [p1, p2] = map (flip param v) [1..2]
            cond = get p1 v /= 0
            next = if cond then jump (get p2 v) else tick 3
        in  next v

op6 :: Vm -> Vm
op6 v = let [p1, p2] = map (flip param v) [1..2]
            cond = get p1 v == 0
            next = if cond then jump (get p2 v) else tick 3
        in  next v

op7 :: Vm -> Vm
op7 v = let [p1, p2, p3] = map (flip param v) [1..3]
            n = if get p1 v < get p2 v then 1 else 0
        in  tick 4 . set n p3 $ v

op8 :: Vm -> Vm
op8 v = let [p1, p2, p3] = map (flip param v) [1..3]
            n = if get p1 v == get p2 v then 1 else 0
        in  tick 4 . set n p3 $ v

op9 :: Vm -> Vm
op9 v = let p1 = param 1 v in tick 2 . adjustBase (get p1 v) $ v

run' :: [Int] -> Vm -> Writer [Int] Vm
run' inputs v@Vm { .. } = case opcode v of
    1  -> run' inputs $ op1 v
    2  -> run' inputs $ op2 v
    3  -> run' (tail inputs) $ op3 (head inputs) v
    4  -> (writer $ op4 v) >>= run' inputs
    5  -> run' inputs $ op5 v
    6  -> run' inputs $ op6 v
    7  -> run' inputs $ op7 v
    8  -> run' inputs $ op8 v
    9  -> run' inputs $ op9 v
    99 -> return v
    x -> error $ "Unknown opcode " ++ show x ++ " at " ++ show pc

run :: [Int] -> Vm -> ([Int], Vm)
run inputs v = swap . runWriter $ run' inputs v
