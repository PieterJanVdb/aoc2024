module Day8 (part1, part2) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Utils.Grid (Coord, Grid, invertGridWithFilter, makeGrid)
import Utils.Lists (pairs)

projectAntenna :: (Coord, Coord) -> Coord
projectAntenna ((r1, c1), (r2, c2)) = next
  where
    dr = abs (r1 - r2)
    dc = abs (c1 - c2)
    next =
      ( if r2 > r1 then r2 + dr else if r1 == r2 then r2 else r2 - dr,
        if c2 > c1 then c2 + dc else if c1 == c2 then c2 else c2 - dc
      )

uniqueAntinodeCount :: Grid Char -> [Coord] -> Int
uniqueAntinodeCount grid antinodes = Map.size $ Map.filter (== '#') gridWithAntinodes
  where
    gridWithAntinodes = foldr (Map.adjust (const '#')) grid antinodes

getFrequencyPairs :: Grid Char -> [(Coord, Coord)]
getFrequencyPairs grid = concat $ Map.map pairs $ invertGridWithFilter (/= '.') grid

part1 :: [String] -> String
part1 input = show (uniqueAntinodeCount grid antinodes)
  where
    grid = makeGrid input
    getAntinodes (x, y) = [projectAntenna (x, y), projectAntenna (y, x)]
    antinodes = concatMap getAntinodes (getFrequencyPairs grid)

part2 :: [String] -> String
part2 input = show (uniqueAntinodeCount grid antinodes)
  where
    grid = makeGrid input
    projectByHarmonics x y =
      map snd $
        takeWhile (\(_, a) -> isJust (Map.lookup a grid)) $
          iterate (\(x, y) -> (y, projectAntenna (x, y))) (x, y)
    getAntinodes (x, y) = projectByHarmonics x y ++ projectByHarmonics y x
    antinodes = concatMap getAntinodes (getFrequencyPairs grid)
