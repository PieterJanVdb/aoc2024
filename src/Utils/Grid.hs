module Utils.Grid
  ( Grid,
    Coord,
    Direction (..),
    makeGrid,
    makeGridWith,
    invertGrid,
    invertGridWithFilter,
    neighbour,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

type Coord = (Int, Int)

type Grid a = Map Coord a

data Direction = N | E | S | W deriving (Enum, Eq, Ord, Show)

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

neighbour :: Coord -> Direction -> Coord
neighbour (r, c) = \case N -> (r - 1, c); E -> (r, c + 1); S -> (r + 1, c); W -> (r, c - 1)
