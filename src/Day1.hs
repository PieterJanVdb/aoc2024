module Day1 (part1, part2) where

import Data.List (sort)
import Utils.Lists (occurrences)
import Utils.String (toInt)

lists :: [String] -> ([Int], [Int])
lists = unzip . map ((\x -> (head x, last x)) . map toInt . words)

distances :: ([Int], [Int]) -> [(Int, Int)]
distances (l, r) = zip (sort l) (sort r)

totalDistance :: [(Int, Int)] -> Int
totalDistance = sum . map (abs . uncurry (-))

similarityScore :: ([Int], [Int]) -> Int
similarityScore (l, r) = sum . map (\x -> x * occurrences x r) $ l

part1 :: [String] -> String
part1 = show . totalDistance . distances . lists

part2 :: [String] -> String
part2 = show . similarityScore . lists
