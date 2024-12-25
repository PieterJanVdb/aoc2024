module Day25 (part1) where

import Data.List (foldl')
import Debug.Trace
import Utils.Lists (splitWhen)

type Schematic = (Int, Int, Int, Int, Int)

parseSchematic :: [String] -> Schematic -> Schematic
parseSchematic [] schema = schema
parseSchematic (x : xs) (a, b, c, d, e) = parseSchematic xs updated
  where
    updated =
      ( a + incr (x !! 0),
        b + incr (x !! 1),
        c + incr (x !! 2),
        d + incr (x !! 3),
        e + incr (x !! 4)
      )
    incr = \case '#' -> 1; '.' -> 0

parse :: String -> (Int, [Schematic], [Schematic])
parse input =
  (\(ls, ks) -> ((\x -> x - 2) $ length $ head schemas, ls, ks)) $
    foldl'
      ( \(locks, keys) schema -> case head schema of
          "....." ->
            let key = parseSchematic (drop 1 $ reverse schema) (0, 0, 0, 0, 0)
             in (locks, key : keys)
          "#####" ->
            let lock = parseSchematic (drop 1 schema) (0, 0, 0, 0, 0)
             in (lock : locks, keys)
      )
      ([], [])
      schemas
  where
    schemas = map (filter (/= "")) $ splitWhen (\_ b -> b == "") (lines input)

opens :: Int -> Schematic -> Schematic -> Bool
opens size (la, lb, lc, ld, le) (ka, kb, kc, kd, ke) =
  (ka + la <= size)
    && (kb + lb <= size)
    && (kc + lc <= size)
    && (kd + ld <= size)
    && (ke + le <= size)

nOpens :: Int -> [Schematic] -> [Schematic] -> Int
nOpens size locks keys = length $ concatMap (filter id . (\l -> map (opens size l) keys)) locks

part1 :: String -> String
part1 input = show $ nOpens size locks keys
  where
    (size, locks, keys) = parse input
