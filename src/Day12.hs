module Day12 (part1, part2) where

import Data.List (foldl', partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Utils.Grid (Coord, Grid, makeGrid)

type Plot = (Int, [Coord])

type Garden = Grid Char

data Corner = Outside | Inside (Set Coord) deriving (Eq, Ord, Show)

getPlot :: Garden -> Set Coord -> Set Coord -> [(Int, Coord)]
getPlot garden nodes visited =
  nodesWithPerimeters
    ++ (if null toVisit then [] else getPlot garden toVisit (S.union nodes visited))
  where
    getNeighbours node@(r, c) =
      [ n
        | n <- [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)],
          M.lookup node garden == M.lookup n garden
      ]
    getPerimeter neighbours = 4 - length neighbours
    toVisit = S.fromList (concatMap getNeighbours (S.toList nodes)) S.\\ visited
    nodesWithPerimeters = map (\n -> (getPerimeter . getNeighbours $ n, n)) (S.toList nodes)

getPlots :: Garden -> [Plot]
getPlots garden =
  map (foldl' (\(n, plants) (n', plant) -> (n + n', plant : plants)) (0, [])) $
    snd $
      M.foldlWithKey'
        ( \(seen, plots) coord _ ->
            if S.member coord seen
              then (seen, plots)
              else
                let p = getPlot garden (S.singleton coord) S.empty
                 in (S.union seen (S.fromList $ map snd p), p : plots)
        )
        (S.empty, [])
        garden

getCorner :: [Coord] -> Coord -> (Int -> Int -> Int, Int -> Int -> Int) -> Maybe Corner
getCorner plot p@(r, c) (hOp, vOp)
  | not hasH && not hasV = Just Outside
  | not hasH && hasV =
      if hasD
        then Just (Inside (S.fromList [p, v, d]))
        else Nothing
  | hasH && not hasV =
      if hasD
        then Just (Inside (S.fromList [p, h, d]))
        else Nothing
  | hasH && hasV && not hasD = Just (Inside (S.fromList [p, h, v]))
  | otherwise = Nothing
  where
    h = (r, c `hOp` 1)
    v = (r `vOp` 1, c)
    d = (r `vOp` 1, c `hOp` 1)
    hasH = h `elem` plot
    hasV = v `elem` plot
    hasD = d `elem` plot

getCorners :: [Coord] -> Int
getCorners plot = length outside + length (S.fromList inside)
  where
    allOps = [((-), (-)), ((-), (+)), ((+), (-)), ((+), (+))]
    corners = concatMap (\p -> mapMaybe (getCorner plot p) allOps) plot
    (outside, inside) = partition (== Outside) corners

getPrice :: [Plot] -> Int
getPrice plots = sum $ map (\(n, coords) -> n * length coords) plots

part1 :: [String] -> String
part1 = show . getPrice . getPlots . makeGrid

part2 :: [String] -> String
part2 = show . getPrice . map (\(_, plot) -> (getCorners plot, plot)) . getPlots . makeGrid
