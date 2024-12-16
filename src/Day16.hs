module Day16 (part1, part2) where

import Data.List (elemIndex, foldl', sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (isJust, isNothing, listToMaybe, mapMaybe)
import Data.Set qualified as S
import Debug.Trace (trace)
import Utils.Grid (Coord, Grid, makeGrid)

data Direction = N | E | S | W deriving (Enum, Eq, Show)

data Entry = Entry Char (Maybe Int) Direction deriving (Eq, Show)

data Neighbour = Neighbour Coord Entry Direction deriving (Show)

ex :: String
ex =
  "###############\n\
  \#.......#....E#\n\
  \#.#.###.#.###.#\n\
  \#.....#.#...#.#\n\
  \#.###.#####.#.#\n\
  \#.#.#.......#.#\n\
  \#.#.#####.###.#\n\
  \#...........#.#\n\
  \###.#.#####.#.#\n\
  \#...#.....#.#.#\n\
  \#.#.#.###.#.#.#\n\
  \#.....#...#.#.#\n\
  \#.###.#.#.#.#.#\n\
  \#S..#.....#...#\n\
  \###############"

ex1 :: String
ex1 =
  "#################\n\
  \#...#...#...#..E#\n\
  \#.#.#.#.#.#.#.#.#\n\
  \#.#.#.#...#...#.#\n\
  \#.#.#.#.###.#.#.#\n\
  \#...#.#.#.....#.#\n\
  \#.#.#.#.#.#####.#\n\
  \#.#...#.#.#.....#\n\
  \#.#.#####.#.###.#\n\
  \#.#.#.......#...#\n\
  \#.#.###.#####.###\n\
  \#.#.#...#.....#.#\n\
  \#.#.#.#####.###.#\n\
  \#.#.#.........#.#\n\
  \#.#.#.#########.#\n\
  \#S#.............#\n\
  \#################"

parse :: [String] -> Grid Char
parse = makeGrid

prepare :: Grid Char -> Grid [Entry]
prepare grid = M.map (\c -> [Entry c (if c == 'S' then Just 0 else Nothing) E]) $ M.filter (/= '#') grid

getNeighbours :: Grid [Entry] -> (Coord, Entry) -> [Neighbour]
getNeighbours grid (_, Entry _ Nothing _) = []
getNeighbours grid ((r, c), Entry _ (Just cs) dir) =
  concat $
    mapMaybe
      (\d -> let c = neighbour d in case M.lookup c grid of Just e -> Just $ map (\x -> Neighbour c x d) e; _ -> Nothing)
      (filter (/= inverse dir) (enumFrom N))
  where
    neighbour = \case N -> (r - 1, c); E -> (r, c + 1); S -> (r + 1, c); W -> (r, c - 1)
    inverse dir = case dir of N -> S; E -> W; S -> N; W -> E

getNextUnvisited :: Grid [Entry] -> Maybe (Coord, Entry)
getNextUnvisited grid =
  listToMaybe $
    dropWhile (\(_, Entry _ s _) -> isNothing s) $
      sortBy (\(_, Entry _ s _) (_, Entry _ s' _) -> compare s s') $
        M.foldlWithKey' (\acc k vs -> acc ++ map (k,) vs) [] grid

getScore :: Direction -> Direction -> Int
getScore from to = if from == to then 1 else 1001

-- MAINTAIN ALL POSSIBLE DIRECTIONS WHERE SCORE = SCORE' (TUPLE OF (Int, Direction))
updateNeighbour :: (Grid [Entry], Map Coord [Coord]) -> (Coord, Int, Direction) -> Neighbour -> (Grid [Entry], Map Coord [Coord])
updateNeighbour (unvisited, visited) (source, current, from) n = case n of
  (Neighbour coord (Entry char Nothing dir) to) ->
    ( M.adjust (const [Entry char (Just (current + getScore from to)) to]) coord unvisited,
      M.alter (Just . maybe [source] (source :)) coord visited
    )
  (Neighbour coord e@(Entry char (Just score) dir) to) ->
    let score' = current + getScore from to
        entry' = Entry char (Just (min score' score)) (if score' < score then to else dir)
        newEntry = if (score' - 1000) == score || score' == (score - 1000) then Just $ Entry char (Just score') to else Nothing
     in ( M.adjust (\xs -> let xs' = (entry' : filter (== e) xs) in maybe xs' (: xs') newEntry) coord unvisited,
          if score' <= score then M.alter (Just . maybe [source] (source :)) coord visited else visited
        )

findShortestPaths :: (Grid [Entry], Map Coord [Coord]) -> Maybe (Int, Map Coord [Coord])
findShortestPaths (unvisited, visited) = case getNextUnvisited unvisited of
  Just (_, Entry 'E' (Just s) _) -> Just (s, visited)
  Just (c, e@(Entry _ (Just current) from)) -> findShortestPaths (M.adjust (filter (/= e)) c unvisited', visited')
    where
      neighbours = getNeighbours unvisited (c, e)
      (unvisited', visited') = foldl' (\(u, v) n -> updateNeighbour (u, v) (c, current, from) n) (unvisited, visited) neighbours
  _ -> Nothing

extractPlacesCount :: Map Coord [Coord] -> Coord -> Int
extractPlacesCount mapping from = length $ S.fromList $ go from [] (M.lookup from mapping)
  where
    go x visited Nothing = x : visited
    go x visited (Just ns) = concatMap (\n -> go n (x : visited) (M.lookup n mapping)) ns

part1 :: [String] -> String
part1 input = maybe "No paths could be found" (show . fst) (findShortestPaths (prepare $ parse input, M.empty))

solve2 :: [String] -> Grid [Coord]
solve2 input = maybe M.empty snd (findShortestPaths (prepare $ parse input, M.empty))

part2 :: [String] -> String
part2 input = maybe "No paths could be found" (show . (`extractPlacesCount` end) . snd) (findShortestPaths (prepare grid, M.empty))
  where
    grid = parse input
    (end, _) = M.findMax $ M.filter (== 'E') grid
