module Day12 (part1, part2) where

import Data.List (foldl', partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Utils.Grid (Coord, Grid, makeGrid)

data Plot = Plot Int Int [Coord]

instance Semigroup Plot where
  (Plot perimeters corners plants) <> (Plot perimeters' corners' plants') =
    Plot (perimeters + perimeters') (corners + corners') (plants ++ plants')

instance Monoid Plot where mempty = Plot 0 0 mempty

type Garden = Grid Char

data Corner = Outside | Inside (Set Coord) deriving (Eq, Ord, Show)

data CornerDirection = NW | NE | SW | SE deriving (Show, Enum)

getPlot :: Garden -> Set Coord -> Set Coord -> Plot
getPlot garden nodes visited =
  plot
    <> if null toVisit then mempty else getPlot garden toVisit (S.union nodes visited)
  where
    getNeighbours node@(r, c) =
      [ n
        | n <- [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)],
          M.lookup node garden == M.lookup n garden
      ]
    getPerimeter neighbours = 4 - length neighbours
    getCorner garden (r, c) dir
      | p /= garden M.!? h && p /= garden M.!? v = 1
      | p == garden M.!? h && p == garden M.!? v && p /= garden M.!? d = 1
      | otherwise = 0
      where
        p = garden M.!? (r, c)
        (h, v, d) = case dir of
          NW -> ((r, c - 1), (r - 1, c), (r - 1, c - 1))
          NE -> ((r, c + 1), (r - 1, c), (r - 1, c + 1))
          SW -> ((r, c - 1), (r + 1, c), (r + 1, c - 1))
          SE -> ((r, c + 1), (r + 1, c), (r + 1, c + 1))
    getCorners n = foldl' (\c dir -> c + getCorner garden n dir) 0 (enumFrom NW)
    toVisit = S.fromList (concatMap getNeighbours (S.toList nodes)) S.\\ visited
    plot = mconcat $ map (\n -> Plot (getPerimeter . getNeighbours $ n) (getCorners n) [n]) (S.toList nodes)

getPlots :: Garden -> [Plot]
getPlots garden =
  snd $
    M.foldlWithKey'
      ( \(seen, plots) coord _ ->
          if S.member coord seen
            then (seen, plots)
            else
              let p@(Plot _ _ coords) = getPlot garden (S.singleton coord) S.empty
               in (S.union seen (S.fromList coords), p : plots)
      )
      (S.empty, [])
      garden

part1 :: [String] -> String
part1 = show . getPrice . getPlots . makeGrid
  where
    getPrice = sum . map (\(Plot perim _ coords) -> perim * length coords)

part2 :: [String] -> String
part2 = show . getPrice . getPlots . makeGrid
  where
    getPrice = sum . map (\(Plot _ corners coords) -> corners * length coords)
