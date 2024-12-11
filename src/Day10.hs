module Day10 (part1, part2) where

import Data.Char (digitToInt)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe, maybe)
import Data.Set qualified as S
import Utils.Grid (Coord, Grid, makeGridWith)

data Direction = N | E | S | W deriving (Show, Enum, Eq)

trails :: Grid Int -> [Coord] -> Coord -> [[Coord]]
trails grid visited current@(r, c) = case (currentHeight, ascending) of
  (9, []) -> [reverse (current : visited)]
  (_, []) -> []
  (_, xs) -> concatMap (filter (not . null) . trails grid (current : visited)) xs
  where
    previous = case visited of [] -> Nothing; (x : xs) -> Just x
    currentHeight = grid M.! current
    neighbours = filter ((/= previous) . Just) [(r - 1, c), (r, c + 1), (r + 1, c), (r, c - 1)]
    ascending = filter (maybe False ((== 1) . flip (-) currentHeight) . (grid M.!?)) neighbours

solve :: ([[Coord]] -> Int) -> [String] -> Int
solve reducer input = sum scores
  where
    grid = makeGridWith digitToInt input
    zeroes = M.keys $ M.filter (== 0) grid
    scores = map (reducer . trails grid []) zeroes

part1 :: [String] -> String
part1 = show . solve (length . S.fromList . map last)

part2 :: [String] -> String
part2 = show . solve length
