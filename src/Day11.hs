module Day11 (part1, part2) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as M
import Data.List (foldl')

type Freq = IntMap Int

parse :: String -> Freq
parse = foldl (\m s -> let n = read s in M.insert n 1 m) M.empty . words

update :: Freq -> Int -> Int -> Freq
update freq stone count = incStones freq stones
  where
    stones = case stone of
      0 -> [1]
      s -> let n = countDigits s in if even n then splitStone s n else [s * 2024]
    incStones = foldr (M.alter (\case Just n -> Just (n + count); Nothing -> Just count))
    countDigits n = if n == 0 then 0 else 1 + countDigits (div n 10)
    splitStone stone n = let q = 10 ^ (n `div` 2) in [stone `div` q, stone `mod` q]

blinks :: Int -> String -> String
blinks n input = show $ sum $ M.filter (> 0) $ iterate blink (parse input) !! n
  where
    blink = M.foldlWithKey' (\c k v -> if v > 0 then update c k v else c) M.empty

part1 :: String -> String
part1 = blinks 25

part2 :: String -> String
part2 = blinks 75
