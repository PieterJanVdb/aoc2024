module Day4 (part1, part2) where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Utils.Grid (Coord, Grid, makeGrid1)

data Direction = N | NE | E | SE | S | SW | W | NW deriving (Show, Enum)

adjacent :: Grid Char -> Coord -> Direction -> Maybe (Coord, Char)
adjacent grid (x, y) dir = (,) coord <$> get coord
  where
    coord = case dir of
      N -> (x - 1, y)
      NE -> (x - 1, y + 1)
      E -> (x, y + 1)
      SE -> (x + 1, y + 1)
      S -> (x + 1, y)
      SW -> (x + 1, y - 1)
      W -> (x, y - 1)
      NW -> (x - 1, y - 1)
    get coord = Map.lookup coord grid

word :: Grid Char -> Coord -> Char -> Direction -> Bool
word grid coord char dir =
  case adjacent grid coord dir of
    Just (adj, c) | c == char -> case char of
      'M' -> word grid adj 'A' dir
      'A' -> word grid adj 'S' dir
      'S' -> True
    _ -> False

cross :: Grid Char -> Coord -> Bool
cross grid coord = fromMaybe False matches
  where
    check char dir = (\(_, c) -> c == char) <$> adjacent grid coord dir
    nwse = (&&) <$> check 'M' NW <*> check 'S' SE
    nesw = (&&) <$> check 'M' NE <*> check 'S' SW
    swne = (&&) <$> check 'M' SW <*> check 'S' NE
    senw = (&&) <$> check 'M' SE <*> check 'S' NW
    left = (||) <$> nwse <*> senw
    right = (||) <$> nesw <*> swne
    matches = (&&) <$> left <*> right

counts :: (Grid Char -> Coord -> Char -> Int) -> Grid Char -> Int
counts step grid = sum $ Map.mapWithKey (step grid) grid

part1 :: [String] -> String
part1 = show . counts step . makeGrid1
  where
    step grid coord char = case char of
      'X' -> length $ filter id $ map (word grid coord 'M') (enumFrom N)
      _ -> 0

part2 :: [String] -> String
part2 = show . counts step . makeGrid1
  where
    step grid coord char = if (char == 'A') && cross grid coord then 1 else 0
