module Day5 (part1, part2) where

import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Utils.Lists ((!?))
import Utils.String (textToInt)

type Rules = Map Int (Set Int)

type Update = [Int]

parseRules :: T.Text -> Rules
parseRules ruleStr = foldr step Map.empty (T.lines ruleStr)
  where
    insert k v = Map.insertWith (<>) (textToInt k) (Set.singleton (textToInt v))
    step line = uncurry insert (T.splitAt 2 line)

parseUpdates :: T.Text -> [Update]
parseUpdates updateStr = map (map textToInt . T.splitOn ",") (T.lines updateStr)

parse :: String -> (Rules, [Update])
parse input = (parseRules ruleStr, parseUpdates updateStr')
  where
    (ruleStr, updateStr) = T.breakOn "\n\n" (T.pack input)
    updateStr' = T.dropWhile (== '\n') updateStr

valid :: Rules -> Update -> Bool
valid rules [] = True
valid rules (x : xs) = isValid && valid rules xs
  where
    isValid = case Map.lookup x rules of
      Just rule -> Set.disjoint (Set.fromList xs) rule
      Nothing -> True

sumMiddles :: [Update] -> Int
sumMiddles updates = sum (mapMaybe middle updates)
  where
    middle update = update !? (length update `div` 2)

partitionUpdates :: Rules -> [Update] -> ([Update], [Update])
partitionUpdates rules = partition (valid rules . reverse)

reorder :: Rules -> Update -> Update
reorder rules update = sortBy (flip (\x y -> if valid rules [x, y] then EQ else GT)) (reverse update)

part1 :: String -> String
part1 = show . sumMiddles . validRules . parse
  where
    validRules (rules, updates) = fst $ partitionUpdates rules updates

part2 :: String -> String
part2 = show . sumMiddles . reorderInvalidRules . parse
  where
    reorderInvalidRules (rules, updates) = map (reorder rules) (snd $ partitionUpdates rules updates)
