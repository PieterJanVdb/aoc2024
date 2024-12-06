module Day6 (part1, part2) where

import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Utils.Grid (Coord, Grid, makeGrid)

data Field = Guard | Obstruction | Empty deriving (Eq, Show)

data Direction = N | E | S | W deriving (Eq, Ord, Show)

type Lab = Grid Field

data Path
  = Finite (Set Coord)
  | Infinite (Set Coord)
  deriving (Ord, Eq, Show)

parse :: [String] -> Lab
parse = makeGrid parseField
  where
    parseField '.' = Empty
    parseField '#' = Obstruction
    parseField '^' = Guard

findGuard :: Lab -> Coord
findGuard lab = fst $ head $ Map.toList $ Map.filter (== Guard) lab

walk :: Lab -> Coord -> Path
walk lab guard = walk' lab guard N Set.empty
  where
    walk' :: Lab -> Coord -> Direction -> Set (Coord, Direction) -> Path
    walk' lab coord dir path | Set.member (coord, dir) path = Infinite (Set.map fst path)
    walk' lab coord dir path =
      case next of
        Just (nextCoord, nextDir) -> walk' lab nextCoord nextDir (Set.insert (coord, dir) path)
        Nothing -> Finite (Set.insert coord (Set.map fst path))
      where
        go (r, c) d = case d of
          N -> (r - 1, c)
          E -> (r, c + 1)
          S -> (r + 1, c)
          W -> (r, c - 1)
        turn from dir = let c = go from dir in (c, dir) <$ Map.lookup c lab
        continue = go coord dir
        next = case Map.lookup continue lab of
          Just Obstruction -> case dir of
            N -> turn coord E
            E -> turn coord S
            S -> turn coord W
            W -> turn coord N
          Just _ -> Just (continue, dir)
          Nothing -> Nothing

solve1 :: Lab -> Set Coord
solve1 lab = case walk lab (findGuard lab) of
  Finite path -> path
  _ -> Set.empty

part1 :: [String] -> String
part1 = show . length . solve1 . parse

part2 :: [String] -> String
part2 input = show $ length infinite
  where
    lab = parse input
    guard = findGuard lab
    try = Set.delete guard (solve1 lab)
    attempted = Set.map (\x -> walk (Map.update (\_ -> Just Obstruction) x lab) guard) try
    infinite = [p | p@(Infinite _) <- Set.toList attempted]
