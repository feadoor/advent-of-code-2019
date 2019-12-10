{-# Language TupleSections #-}

import Advent

import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Ord
import qualified Data.Set as Set
import Text.Megaparsec (many, single, (<|>))

-- Asteroid utilities

relativePosition :: (Int, Int) -> (Int, Int) -> (Int, Int)
relativePosition (x, y) = (,) <$> (subtract x . fst) <*> (subtract y . snd)

relativeTo :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
relativeTo base = let relativeTo' = map (relativePosition base)
                  in filter (/= (0, 0)) . relativeTo'

gradient :: (Int, Int) -> (Int, Int)
gradient (x, 0)
    | x > 0 = (1, 0)
    | x < 0 = (-1, 0)
gradient (x, y) = let g = gcd x y in (x `div` g, y `div` g)

angle :: (Int, Int) -> Double
angle (x, y) = atan2 (fromIntegral y) (fromIntegral x)

-- Asteroid munging

countVisibleFrom :: (Int, Int) -> [(Int, Int)] -> Int
countVisibleFrom (x, y) = length . Set.fromList . map gradient . relativeTo (x, y)

bestStation :: [(Int, Int)] -> (Int, Int)
bestStation asteroids = maximumBy (comparing $ flip countVisibleFrom asteroids) $ asteroids

destructionOrder :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
destructionOrder (x, y) asteroids = map (\(a, b) -> (a + x, b + y)) flattenedAsteroids
    where flattenedAsteroids = concat $ transpose asteroidsInGradientOrder
          asteroidsInGradientOrder = map (byGradient Map.!) sortedGradients
          sortedGradients = sortBy (comparing $ negate . angle) $ Map.keys byGradient
          byGradient = Map.fromListWith (++) $ map((,) <$> gradient <*> pure) relativeAsteroids
          relativeAsteroids = relativeTo (x, y) asteroids

-- Input parsing

positionsInRow :: String -> [Int]
positionsInRow = map fst . filter ((== '#') . snd) . zip [0..]

coordinatesInRow :: Int -> String -> [(Int, Int)]
coordinatesInRow idx = map (idx,) . positionsInRow

coordinates :: [String] -> [(Int, Int)]
coordinates = concatMap (uncurry coordinatesInRow) . zip [0..]

parseField :: Parser [String]
parseField = parseLines $ many (single '.' <|> single '#')

parseAsteroids :: Parser [(Int, Int)]
parseAsteroids = coordinates <$> parseField

-- Putting it all together

part1 :: [(Int, Int)] -> Int
part1 asteroids = maximum . map (flip countVisibleFrom asteroids) $ asteroids

part2 :: [(Int, Int)] -> Int
part2 asteroids = 100 * y + x
    where (x, y) = destroyed !! 199
          destroyed = destructionOrder base asteroids
          base = bestStation asteroids

readInput = parsedInput parseAsteroids

main = solve readInput part1 part2
