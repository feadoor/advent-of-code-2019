{-# Language RecordWildCards #-}

import Advent

import Control.Monad
import Data.Maybe
import Data.List
import Text.Megaparsec (many, single, sepBy, (<|>))
import Text.Megaparsec.Char (alphaNumChar)

-- Moon utilities

data Moon = Moon { pos :: [Int], vel :: [Int] } deriving Eq

step :: [Moon] -> [Moon]
step moons = map (move . accel moons) moons

move :: Moon -> Moon
move moon = moon { pos = zipWith (+) <$> pos <*> vel $ moon }

accel :: [Moon] -> Moon -> Moon
accel moons moon = moon { vel = zipWith (+) (vel moon) accels }
    where accels = map (sum . map signum) deltas
          deltas = map (map <$> subtract . snd <*> fst) . zip (transpose $ map pos moons) $ pos moon

energy :: Moon -> Int
energy = (*) <$> kinetic <*> potential
    where kinetic   = sum . map abs . vel
          potential = sum . map abs . pos

flattened :: Moon -> [Moon]
flattened = map (\(p,v) -> Moon { pos = [p], vel = [v] }) . (zip <$> pos <*> vel)

period' :: [Moon] -> Maybe Int
period' moons = fmap ((+) 1) $ findIndex (== moons) $ drop 1 $ iterate step moons

period :: [Moon] -> Maybe Int
period moons = foldl1 (liftM2 lcm) periods
    where periods = map period' . transpose . map flattened $ moons

-- Input parsing

parseCoordinate :: Parser Int
parseCoordinate = alphaNumChar <* single '=' *> parseInt

parsePosition :: Parser [Int]
parsePosition = parseCoordinate `sepBy` many (single ',' <|> single ' ')

parseMoon :: Parser Moon
parseMoon = makeMoon <$> (single '<' *> parsePosition <* single '>')

makeMoon :: [Int] -> Moon
makeMoon pos = Moon { pos = pos, vel = map (const 0) pos }

-- Putting it all together

readInput :: IO [Moon]
readInput = parsedLines parseMoon

part1 :: [Moon] -> Int
part1 moons = sum . map energy $ iterate step moons !! 1000

part2 :: [Moon] -> Int
part2 = fromJust . period

main = solve readInput part1 part2