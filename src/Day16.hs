module Day16 (part1, part2) where

import Data.List (elemIndex, foldl', sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Set qualified as S
import Debug.Trace (trace)
import Utils.Grid (Coord, Direction (..), Grid, makeGrid, neighbour)

type Path = [Coord]

type Queue = [(Path, Direction, Int)]

type Seen = Map (Coord, Direction) Int

findSeatsAndScore :: Grid Char -> Maybe (Int, Int)
findSeatsAndScore grid = go grid initQueue M.empty [] maxBound
  where
    (start, _) = M.findMax $ M.filter (== 'S') grid
    initQueue = [([start], E, 0)]

    pop :: Queue -> Maybe ((Path, Direction, Int), [(Path, Direction, Int)])
    pop queue = case sortBy (\(_, _, a) (_, _, b) -> compare a b) queue of
      [] -> Nothing
      (x : xs) -> Just (x, xs)

    go :: Grid Char -> Queue -> Seen -> [Path] -> Int -> Maybe (Int, Int)
    go grid queue seen paths minScore = case pop queue of
      Nothing -> Nothing
      Just ((path@(p : ps), dir, score), queue') ->
        case grid M.! p of
          'E' | score > minScore -> Just (minScore, length $ S.fromList $ concat paths)
          'E' -> go grid queue' seen (path : paths) score
          _ | stop -> go grid queue' seen paths minScore
          _ -> go grid (toProcess ++ queue') seen' paths minScore
        where
          stop = maybe False (< score) (M.lookup (p, dir) seen)
          seen' = M.alter (Just . const score) (p, dir) seen
          toProcess =
            catMaybes
              [ (neighbour p dir : path, dir, score + 1) <$ M.lookup (neighbour p dir) grid,
                Just (path, turnLeft dir, score + 1000),
                Just (path, turnRight dir, score + 1000)
              ]
          turnLeft = \case N -> W; E -> N; S -> E; W -> S
          turnRight = \case N -> E; E -> S; S -> W; W -> N

part1 :: [String] -> String
part1 input = maybe "No paths could be found" (show . fst) (findSeatsAndScore grid)
  where
    grid = M.filter (/= '#') $ makeGrid input

part2 :: [String] -> String
part2 input = maybe "No paths could be found" (show . snd) (findSeatsAndScore grid)
  where
    grid = M.filter (/= '#') $ makeGrid input
