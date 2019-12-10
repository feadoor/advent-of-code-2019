{-# Language TupleSections #-}

import Advent

import Data.Function
import Data.List
import Data.Map (Map)
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
bestStation as = maximumBy (comparing $ flip countVisibleFrom as) $ as

byGradient :: [(Int, Int)] -> Map (Int, Int) [(Int, Int)]
byGradient as = Map.map (sortBy $ comparing $ uncurry ((,) `on` abs)) mapByGradient
    where mapByGradient  = Map.fromListWith (++) asWithGradient
          asWithGradient = map ((,) <$> gradient <*> pure) as

destructionOrder :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
destructionOrder (x, y) as = map (\(a, b) -> (a + x, b + y)) flattenedAsteroids
    where flattenedAsteroids = concat $ transpose asteroidsInGradientOrder
          asteroidsInGradientOrder = map (gradientMap Map.!) sortedGradients
          sortedGradients = sortBy (comparing $ negate . angle) $ Map.keys gradientMap
          gradientMap = byGradient $ relativeTo (x, y) as

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
part1 as = maximum . map (flip countVisibleFrom as) $ as

part2 :: [(Int, Int)] -> Int
part2 as = let (x, y) = destroyed !! 199
               destroyed = destructionOrder base as
               base = bestStation as
           in  100 * y + x

readInput = parsedInput parseAsteroids

main = solve readInput part1 part2
