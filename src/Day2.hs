module Day2 (part1, part2) where

import Utils.String (toInt)

data Direction = Increasing | Decreasing

variations' :: [Int] -> [Int] -> [[Int]]
variations' _ [] = []
variations' prev (x : xs) = (reverse prev ++ xs) : variations' (x : prev) xs

variations :: [Int] -> [[Int]]
variations = variations' []

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

safe :: [Int] -> Bool
safe = safe' Nothing

levels :: [String] -> [[Int]]
levels = fmap (fmap toInt . words)

part1 :: [String] -> String
part1 = show . length . filter safe . levels

part2 :: [String] -> String
part2 = show . length . filter (\x -> any safe (x : variations x)) . levels
