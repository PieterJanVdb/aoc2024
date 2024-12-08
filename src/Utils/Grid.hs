module Utils.Grid (Grid, Coord, makeGrid, makeGridWith, invertGrid, invertGridWithFilter) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

type Coord = (Int, Int)

type Grid a = Map Coord a

makeGridWith :: (a -> b) -> [[a]] -> Grid b
makeGridWith f xs = Map.fromList $ concatMap (uncurry makeRow) (zip [0 ..] xs)
  where
    makeRow idx = zipWith (\col a -> ((idx, col), f a)) [0 ..]

makeGrid :: [[a]] -> Grid a
makeGrid = makeGridWith id

invertGridWithFilter :: (Ord a) => (a -> Bool) -> Grid a -> Map a [Coord]
invertGridWithFilter filter g = Map.fromListWith (++) [(v, [k]) | (k, v) <- Map.toList g, filter v]

invertGrid :: (Ord a) => Grid a -> Map a [Coord]
invertGrid = invertGridWithFilter (const True)
