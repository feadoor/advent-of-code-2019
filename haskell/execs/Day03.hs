import Advent

import Control.Applicative
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Megaparsec (sepBy, single)

-- Data types and utilities

data Direction = U | D | R | L
data Instruction = Instruction Direction Int

type Point = (Int, Int)

move :: Direction -> Point -> Point
move dir (x, y) = case dir of
    U -> (x, y + 1)
    D -> (x, y - 1)
    R -> (x + 1, y)
    L -> (x - 1, y)

manhattan :: Point -> Int
manhattan (x, y) = abs x + abs y

-- Determine the path of a wire around the plane

steps :: Instruction -> [Direction]
steps (Instruction d n) = replicate n d

positions :: [Instruction] -> [Point]
positions = drop 1 . scanl (flip move) (0, 0) . concatMap steps

indexedPositions :: [Instruction] -> Map Point Int
indexedPositions = Map.fromListWith min . flip zip [1..] . positions

-- Input parsing

parseDirection :: Parser Direction
parseDirection = asum [U <$ single 'U', D <$ single 'D', R <$ single 'R', L <$ single 'L']

parseInstruction :: Parser Instruction
parseInstruction = liftA2 Instruction parseDirection parseInt

parseWire :: Parser [Instruction]
parseWire = parseInstruction `sepBy` single ','

-- Putting it all together

part1 :: [[Instruction]] -> Int
part1 = minimum . Set.map manhattan . foldr1 Set.intersection . map (Map.keysSet . indexedPositions)

part2 :: [[Instruction]] -> Int
part2 = minimum . foldr1 (Map.intersectionWith (+)) . map indexedPositions

readInput :: IO [[Instruction]]
readInput = parsedLines parseWire

main = solve readInput part1 part2
