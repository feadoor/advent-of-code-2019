{-# Language RecordWildCards #-}

module Advent.Intcode (
    Memory, Vm(..), parseMem, vm,
    Step(..), step, Effect(..), effect, runner
) where

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

op3 :: Vm -> Int -> Vm
op3 v input = let p1 = param 1 v in tick 2 . set input p1 $ v

op4 :: Vm -> (Int, Vm)
op4 v = let p1 = param 1 v in (get p1 v, tick 2 v)

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

-- Running a VM one step at a time

data Step = Step Vm | StepIn (Int -> Vm) | StepOut (Int, Vm) | StepHalt

step :: Vm -> Step
step v = case opcode v of
    1  -> Step     $ op1 v
    2  -> Step     $ op2 v
    3  -> StepIn   $ op3 v
    4  -> StepOut  $ op4 v
    5  -> Step     $ op5 v
    6  -> Step     $ op6 v
    7  -> Step     $ op7 v
    8  -> Step     $ op8 v
    9  -> Step     $ op9 v
    99 -> StepHalt
    x  -> error $ "Unknown opcode " ++ show x ++ " at " ++ show (pc v)

-- Representing a VM as a sequence of input and output instructions

data Effect = Input (Int -> Effect) | Output (Int, Effect) | Halt

effect :: Vm -> Effect
effect vm = case step vm of
    Step vm'           -> effect vm'
    StepIn f           -> Input (effect . f)
    StepOut (out, vm') -> Output (out, effect vm')
    StepHalt           -> Halt

-- Representing a VM as a function from inputs to outputs

runner' :: Effect -> [Int] -> [Int]
runner' effect inputs = case effect of
    Input f               -> runner' (f $ head inputs) $ tail inputs
    Output (out, effect') -> out : runner' effect' inputs
    Halt                  -> []

runner :: Vm -> [Int] -> [Int]
runner vm = runner' $ effect vm
