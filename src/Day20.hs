module Day20 (part1, part2) where

import Data.List (foldl')
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe, mapMaybe)
import Utils.Grid (Coord, Direction (..), Grid, makeGridWith, neighbours)

data Space = Wall | Free | Start | End deriving (Show, Eq)

parse :: [String] -> Grid Space
parse = makeGridWith space
  where
    space = \case
      '.' -> Free
      '#' -> Wall
      'S' -> Start
      'E' -> End

shortestPath :: Grid Space -> Maybe [Coord]
shortestPath grid = go start [start]
  where
    (start, _) = M.findMax $ M.filter (== Start) grid

    go coord visited = case M.lookup coord grid of
      Just End -> Just visited
      _ -> listToMaybe $ mapMaybe (\c -> go c (c : visited)) toVisit
        where
          toVisit =
            filter (`notElem` visited) $
              mapMaybe (\c -> c <$ M.lookup c grid) (neighbours coord)

manhdst :: Coord -> Coord -> Int
manhdst (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

findCheats :: Grid Space -> Int -> [Coord] -> Int
findCheats grid maxDist path = foldl' (\cs (p, i) -> cs + find p i) 0 indexed
  where
    indexed = zip path [0 ..]
    len = length path

    find coord idx =
      length
        $ filter
          ( \(i, d) ->
              d <= maxDist
                && (i - idx) > d
                && (len - (len - (i - idx) + d)) >= 100
          )
        $ map (\(x, i) -> (i, manhdst coord x)) indexed

solve :: [String] -> Int -> Maybe Int
solve input n = findCheats grid n <$> shortestPath grid
  where
    grid = M.filter (/= Wall) $ parse input

part1 :: [String] -> String
part1 input = maybe "uhoh" show (solve input 2)

part2 :: [String] -> String
part2 input = maybe "uhoh" show (solve input 20)
