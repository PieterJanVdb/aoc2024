module Day2 (part1, part2) where

import Utils.String (toInt)

data Direction = Increasing | Decreasing

variations :: [Int] -> [[Int]]
variations = variations' ([], [])

variations' :: ([Int], [[Int]]) -> [Int] -> [[Int]]
variations' (_, acc) [] = acc
variations' (prev, vars) [x] = variations' (prev ++ [x], prev : vars) []
variations' (prev, vars) (x : xs) = variations' (prev ++ [x], (prev ++ xs) : vars) xs

safe :: [Int] -> Bool
safe = safe' Nothing

safe' :: Maybe Direction -> [Int] -> Bool
safe' _ [] = True
safe' _ [_] = True
safe' dir (x : y : xs)
  | (y > x) && (y - x) <= 3 = case dir of
      Just Decreasing -> False
      _ -> safe' (Just Increasing) (y : xs)
  | (x > y) && (x - y) <= 3 = case dir of
      Just Increasing -> False
      _ -> safe' (Just Decreasing) (y : xs)
  | otherwise = False

levels :: [String] -> [[Int]]
levels = fmap (fmap toInt . words)

part1 :: [String] -> String
part1 = show . length . filter safe . levels

part2 :: [String] -> String
part2 = show . length . filter (\x -> any safe (x : variations x)) . levels
