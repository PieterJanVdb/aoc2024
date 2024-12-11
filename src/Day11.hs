module Day11 (part1, part2) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as M
import Data.List (foldl')

type Cache = IntMap Int

parse :: String -> Cache
parse = foldl (\m s -> let n = read s in M.insert n 1 m) M.empty . words

update :: Cache -> Int -> Int -> Cache
update cache stone count = incStones (decStone stone cache) stones
  where
    stones = case stone of
      0 -> [1]
      s | even (countDigits s) -> splitStone s
      s -> [s * 2024]
    decStone = M.adjust (\n -> n - count)
    incStones = foldr (M.alter (\case Just n -> Just (n + count); Nothing -> Just count))
    countDigits n = if n == 0 then 0 else 1 + countDigits (div n 10)
    splitStone stone =
      let stoneS = show stone
       in (\(x, y) -> [read x, read y]) $
            splitAt ((length stoneS + 1) `div` 2) stoneS

blinks :: Int -> String -> String
blinks n input = show $ sum $ M.filter (> 0) $ iterate blink (parse input) !! n
  where
    blink cache = M.foldlWithKey' (\c k v -> if v > 0 then update c k v else c) cache cache

part1 :: String -> String
part1 = blinks 25

part2 :: String -> String
part2 = blinks 75
