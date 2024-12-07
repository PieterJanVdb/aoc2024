module Day7 (part1, part2) where

import Data.Set qualified as Set
import Data.Text qualified as T
import Utils.String (textToInt, toInt)

type Equation = (Int, [Int])

parse :: String -> Equation
parse line = (textToInt result, nrs)
  where
    (result, rest) = T.breakOn ":" (T.pack line)
    nrs = map textToInt (drop 1 (T.words rest))

isPossible :: [Int -> Int -> Int] -> Equation -> Bool
isPossible operators (res, nrs) = Just res `elem` collect res nrs
  where
    collect :: Int -> [Int] -> [Maybe Int]
    collect res [x] = [Just x]
    collect res (x : y : xs) = concatMap applyOp operators
      where
        applyOp op = let res' = op x y in if res' > res then [Nothing] else collect res (res' : xs)

solve :: [String] -> [Int -> Int -> Int] -> String
solve input operators = show result
  where
    equations = map parse input
    possible = filter (isPossible operators) equations
    result = foldr (\x acc -> fst x + acc) 0 possible

concatenate :: Int -> Int -> Int
concatenate x y = x * pow 10 y + y
  where
    pow p a = if a >= p then pow (p * 10) a else p

part1 :: [String] -> String
part1 input = solve input [(+), (*)]

part2 :: [String] -> String
part2 input = solve input [(+), (*), concatenate]
