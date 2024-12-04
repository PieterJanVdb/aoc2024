module Utils.Grid (Grid, Coord, makeGrid) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

type Coord = (Int, Int)

type Grid a = Map Coord a

makeRow :: Int -> [a] -> [(Coord, a)]
makeRow idx = zipWith (\col a -> ((idx, col), a)) [0 ..]

makeGrid :: [[a]] -> Grid a
makeGrid xs = Map.fromList $ concatMap (uncurry makeRow) (zip [0 ..] xs)
