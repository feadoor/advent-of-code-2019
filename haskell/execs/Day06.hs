import Advent

import Data.Functor ((<&>))
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Monoid
import Data.Set (Set, difference, union)
import qualified Data.Set as Set
import Text.Megaparsec (many, single)
import Text.Megaparsec.Char (alphaNumChar)

-- Tree manipulation

createTree :: [(String, String)] -> Map String String
createTree os = Map.fromList [(c, p) | (p, c) <- os]

accumulation :: (Ord a, Monoid m) => (a -> m) -> Map a a -> Map a m
accumulation f t = t <&> \p -> case Map.lookup p (accumulation f t) of
    Nothing -> f p
    Just acc -> f p `mappend` acc

-- Input parsing

parseName :: Parser String
parseName = many alphaNumChar

parseOrbit :: Parser (String, String)
parseOrbit = (,) <$> parseName <* single ')' <*> parseName

parseRawTree :: Parser [(String, String)]
parseRawTree = parseLines parseOrbit

parseTree :: Parser (Map String String)
parseTree = createTree <$> parseRawTree

-- Putting it all together

part1 :: Map String String -> Int
part1 = getSum . sum . accumulation (const $ Sum 1)

part2 :: Map String String -> Int
part2 t = let ancestors = accumulation (pure :: String -> [String]) t
              youAncestors = Set.fromList $ ancestors ! "YOU"
              sanAncestors = Set.fromList $ ancestors ! "SAN"
              uniqueAncestors = (youAncestors `difference` sanAncestors) `union` (sanAncestors `difference` youAncestors)
        in length uniqueAncestors

readInput :: IO (Map String String)
readInput = parsedInput parseTree

main = solve readInput part1 part2
