module Day18 (part1, part2) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List (foldl')
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T
import Utils.Grid (Coord, Direction (..), Grid, makeGrid, neighbours)
import Utils.Lists ((!?))
import Utils.String (toInt)

bounds = 70

corrupted = 1024

parse :: [String] -> Grid Char
parse input = foldl' (flip M.delete) grid coords
  where
    grid = makeGrid (replicate (bounds + 1) (replicate (bounds + 1) '.'))
    coords =
      map
        (\i -> let (x, y) = break (== ',') i in (toInt (drop 1 y), toInt x))
        input

data State = State
  { visited :: HashSet Coord,
    distances :: HashMap Coord Int,
    queue :: Set (Int, Coord)
  }

solve :: Grid Char -> Maybe Int
solve grid = processQueue initialState
  where
    start = (0, 0)
    end = (bounds, bounds)

    initialVisited = HS.empty
    initialDistances = HM.singleton start 0
    initialQueue = S.singleton (0, start)
    initialState = State initialVisited initialDistances initialQueue

    processQueue state@(State v0 d0 q0) = case S.minView q0 of
      Nothing -> Nothing
      Just ((score, node), q1) ->
        if node == end
          then Just score
          else
            if HS.member node v0
              then processQueue (state {queue = q1})
              else
                let v1 = HS.insert node v0
                    toVisit =
                      filter (\c -> not (HS.member c v1)) $
                        mapMaybe (\c -> c <$ M.lookup c grid) (neighbours node)
                 in processQueue $ foldl (foldNeighbour node) (State v1 d0 q1) toVisit

    foldNeighbour current state@(State v1 d0 q1) neighbour =
      let alt = (d0 HM.! current) + 1
          dist = HM.lookup neighbour d0
       in if maybe True (alt <) dist
            then State v1 (HM.insert neighbour alt d0) (S.insert (alt, neighbour) q1)
            else state

part1 :: [String] -> String
part1 input = maybe "No paths could be found" show (solve grid)
  where
    grid = parse (take corrupted input)

part2 :: [String] -> String
part2 input =
  maybe "Could not find a blocking byte" (T.unpack . T.strip . T.pack) $
    foldr
      ( \n r -> case solve (parse (good ++ take n toTry)) of
          Just _ -> r
          Nothing -> toTry !? (n - 1)
      )
      Nothing
      [1 .. (length toTry - 1)]
  where
    (good, toTry) = splitAt corrupted input
