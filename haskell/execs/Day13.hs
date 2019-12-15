import Advent
import Advent.Intcode

import qualified Data.Sequence as Seq

-- Utilities

everyNth :: Int -> [Int] -> [Int]
everyNth n xs = case drop (n - 1) xs of
    (x:xs') -> x : everyNth n xs'
    []      -> []

-- Breakout bot

data BotState  = BotState { ball :: Maybe Int, paddle :: Maybe Int, score :: Int }

paddleDirection :: BotState -> Int
paddleDirection BotState { ball = Just b, paddle = Just p} = signum (b - p)
paddleDirection _ = 0

bot :: BotState -> Effect -> Int
bot state effect = case effect of

    Halt    -> score state
    Input f -> bot state $ f $ paddleDirection state

    Output (-1, Output (0, Output (score', effect'))) -> bot state { score  = score' } effect'
    Output (paddle', Output (_, Output (3, effect'))) -> bot state { paddle = Just paddle' } effect'
    Output (ball', Output (_, Output (4, effect')))   -> bot state { ball   = Just ball' } effect'

    Output (_, Output (_, Output (_, effect'))) -> bot state effect'

-- Putting it all together

part1 :: Memory -> Int
part1 = length . filter (== 2) . everyNth 3 . ($ []) . runner . vm

part2 :: Memory -> Int
part2 = bot newState . effect . vm . Seq.update 0 2
    where newState = BotState { ball = Nothing, paddle = Nothing, score = 0 }

readInput :: IO Memory
readInput = parsedInput parseMem

main = solve readInput part1 part2