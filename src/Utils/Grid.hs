module Utils.Grid (Grid, Coord, makeGrid1, makeGrid) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

type Coord = (Int, Int)

type Grid a = Map Coord a

makeGrid :: (a -> b) -> [[a]] -> Grid b
makeGrid f xs = Map.fromList $ concatMap (uncurry makeRow) (zip [0 ..] xs)
  where
    makeRow idx = zipWith (\col a -> ((idx, col), f a)) [0 ..]

makeGrid1 :: [[a]] -> Grid a
makeGrid1 = makeGrid id
