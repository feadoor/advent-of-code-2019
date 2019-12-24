import Advent

import Data.Graph
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Text.Megaparsec (many, single, sepBy, (<|>))
import Text.Megaparsec.Char (alphaNumChar, string)

-- Utilities

first :: (a, b, c) -> a
first (a, _, _) = a

-- Alchemy

type Ingredient = (Int, String)

type Recipe = (Ingredient, [Ingredient])

type Supplies = Map String Int

topologicalOrder :: [Recipe] -> [Recipe]
topologicalOrder rs = map (first . nlookup) orderedVertices
    where orderedVertices = topSort graph
          (graph, nlookup, _) = graphFromEdges edgeList
          edgeList = map (\r -> (r, snd $ fst r, map snd $ snd r)) rs

update :: Int -> Ingredient -> Supplies -> Supplies
update qty i = Map.alter f $ snd i
    where f Nothing  = Just $ qty * fst i
          f (Just x) = Just $ qty * fst i + x

transmute :: Recipe -> Supplies -> Supplies
transmute r m = Map.delete name $ (foldl1 (.) updates) m
    where updates = map (update qty) $ snd r
          qty = ((m ! name) + amt - 1) `div` amt
          (amt, name) = fst r

oreRequiredForFuel :: [Recipe] -> Int -> Int
oreRequiredForFuel rs fuel = end ! "ORE"
    where end = foldl1 (flip (.)) (map transmute recipes) $ start
          start = Map.insert "FUEL" fuel Map.empty
          recipes = topologicalOrder rs

maximumFuelFromOre :: [Recipe] -> Int -> Int
maximumFuelFromOre rs ore = foldl update 0 $ reverse steps
  where steps = takeWhile ((<= ore) . oreRequiredForFuel rs) $ iterate (* 2) 1
        update v s = if oreRequiredForFuel rs (v + s) <= ore then v + s else v

-- Input parsing

parseIngredient :: Parser Ingredient
parseIngredient = (,) <$> parseInt <* single ' ' <*> many alphaNumChar

parseIngredients :: Parser [Ingredient]
parseIngredients = (:) <$> parseIngredient <*> many (string ", " *> parseIngredient)

parseRecipe :: Parser Recipe
parseRecipe = flip (,) <$> parseIngredients <* string " => " <*> parseIngredient

-- Putting it all together

readInput :: IO [Recipe]
readInput = parsedLines parseRecipe

part1 :: [Recipe] -> Int
part1 rs = oreRequiredForFuel rs 1

part2 :: [Recipe] -> Int
part2 rs = maximumFuelFromOre rs 1000000000000

main = solve readInput part1 part2
