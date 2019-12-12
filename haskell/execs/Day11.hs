{-# Language RecordWildCards #-}

import Advent
import Advent.Intcode

import Data.Set (Set)
import qualified Data.Set as Set

-- Utilities

data Rotation  = CW | CCW
data Direction = U | D | L | R
data Colour    = B | W

turn :: Rotation -> Direction -> Direction
turn CW  U = R
turn CW  R = D
turn CW  D = L
turn CW  L = U
turn CCW U = L
turn CCW L = D
turn CCW D = R
turn CCW R = U

move :: Direction -> (Int, Int) -> (Int, Int)
move U (x, y) = (x, y - 1)
move D (x, y) = (x, y + 1)
move L (x, y) = (x - 1, y)
move R (x, y) = (x + 1, y)

colourCode :: Colour -> Int
colourCode B = 0
colourCode W = 1

colour :: Int -> Colour
colour 0 = B
colour 1 = W
colour x = error $ "Invalid colour code " ++ show x

rotation :: Int -> Rotation
rotation 0 = CCW
rotation 1 = CW
rotation x = error $ "Invalid rotation code " ++ show x

pairs :: [a] -> [(a, a)]
pairs (x:y:xs) = (x, y) : pairs xs
pairs _        = []

-- Steps taken by the robot

type Position = (Int, Int)
type Field = Set Position
data RobotState = RobotState { pos :: Position, dir :: Direction, field :: Field }

paint :: Colour -> Position -> Field -> Field
paint B pos = Set.delete pos
paint W pos = Set.insert pos

inputColour :: Position -> Field -> Colour
inputColour pos field = if Set.member pos field then W else B

step :: (Colour, Rotation) -> RobotState -> RobotState
step (c, r) state@RobotState { .. } = RobotState { pos = newPos, dir = newDir, field = newField }
    where newPos   = move newDir pos
          newDir   = turn r dir
          newField = paint c pos field

steps :: [(Colour, Rotation)] -> RobotState -> [RobotState]
steps instructions state = state : steps' instructions state
    where steps' (x:xs) state' = steps xs (step x state')
          steps' []     state' = []

instructions :: Vm -> [Colour] -> [(Colour, Rotation)]
instructions vm inputs = [(colour x, rotation y) | (x, y) <- pairs outputs]
    where outputs = run (map colourCode inputs) vm

allStates :: Vm -> RobotState -> [RobotState]
allStates vm startState = states
    where states  = steps outputs startState
          outputs = instructions vm inputs
          inputs  = map (inputColour <$> pos <*> field) states

-- Putting it all together

part1 :: Memory -> Int
part1 memory = Set.size positions
    where positions  = Set.fromList $ map pos $ allStates (vm memory) startState
          startState = RobotState { pos = (0, 0), dir = U, field = Set.empty }

part2 :: Memory -> String
part2 memory = unlines [[pixel x y | x <- [minx..maxx]] | y <- [miny..maxy]]
    where pixel x y  = if Set.member (x, y) painted then 'X' else ' '
          minx = minimum . Set.map fst $ painted
          maxx = maximum . Set.map fst $ painted
          miny = minimum . Set.map snd $ painted
          maxy = maximum . Set.map snd $ painted
          painted    = field . last $ allStates (vm memory) startState
          startState = RobotState { pos = (0, 0), dir = U, field = Set.insert (0, 0) Set.empty }

readInput :: IO Memory
readInput = parsedInput parseMem

main = solve readInput part1 (PlainString . part2)
