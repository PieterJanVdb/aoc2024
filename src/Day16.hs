module Day16 (part1, part2) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List (sortBy)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as S
import Utils.Grid (Coord, Direction (..), Grid, makeGrid, neighbour, turnLeft, turnRight)

data State = State
  { distanceMap :: HashMap (Coord, Direction) Int,
    queue :: Set (Int, [Coord], Direction),
    seats :: HashSet Coord,
    minScore :: Int
  }

solve :: Grid Char -> Maybe (Int, Int)
solve grid = processQueue initialState
  where
    (start, _) = M.findMax $ M.filter (== 'S') grid
    (end, _) = M.findMax $ M.filter (== 'E') grid

    initialDistances = HM.singleton (start, E) 0
    initialQueue = S.singleton (0, [start], E)
    initialMinScore = maxBound
    initialSeats = HS.empty
    initialState = State initialDistances initialQueue initialSeats initialMinScore

    processQueue state@(State d0 q0 s0 minScore) = case S.minView q0 of
      Nothing -> Nothing
      Just ((score, path@(p : ps), dir), q1) ->
        if p == end
          then
            if score > minScore
              then Just (minScore, length s0)
              else
                let s1 = HS.union s0 (HS.fromList path)
                 in processQueue (state {queue = q1, seats = s1, minScore = score})
          else
            let neighbours = getNeighbours p dir score
             in processQueue $ foldl (foldNeighbour (path, dir)) (State d0 q1 s0 minScore) neighbours

    foldNeighbour (path@(current : _), from) state@(State d0 q1 s0 minScore) (neighbour, cost, to) =
      let altDistance = d0 HM.! (current, from) + cost
          neighbourDistance = HM.lookup (neighbour, to) d0
       in if maybe True (\x -> altDistance <= x - 2000 || altDistance - 2000 <= x) neighbourDistance
            then State (HM.insert (neighbour, to) altDistance d0) (S.insert (altDistance, neighbour : path, to) q1) s0 minScore
            else state

    getNeighbours current dir score =
      let straight = neighbour current dir
          left = neighbour current (turnLeft dir)
          right = neighbour current (turnRight dir)
       in catMaybes
            [ (straight, 1, dir) <$ M.lookup straight grid,
              (left, 1001, turnLeft dir) <$ M.lookup left grid,
              (right, 1001, turnRight dir) <$ M.lookup right grid
            ]

part1 :: [String] -> String
part1 input = maybe "No paths could be found" (show . fst) (solve grid)
  where
    grid = M.filter (/= '#') $ makeGrid input

part2 :: [String] -> String
part2 input = maybe "No paths could be found" (show . snd) (solve grid)
  where
    grid = M.filter (/= '#') $ makeGrid input
