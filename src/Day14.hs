module Day14 (part1, part2) where

import Data.Either (rights)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List (foldl', sort)
import Text.Parsec (parse)
import Text.Parsec.Char (char, digit, string)
import Text.Parsec.Language (haskellDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (integer, makeTokenParser)
import Utils.Lists (splitWhen)

type Position = (Int, Int)

type Quadrants = (Int, Int, Int, Int)

type Velocity = (Int, Int)

data Robot = Robot Position Velocity deriving (Show)

width = 101

height = 103

lexer = makeTokenParser haskellDef

robot :: Parser Robot
robot =
  (\x y xv yv -> Robot (fromInteger x, fromInteger y) (fromInteger xv, fromInteger yv))
    <$> (string "p=" *> integer lexer <* char ',')
    <*> (integer lexer <* string "v=")
    <*> (integer lexer <* char ',')
    <*> integer lexer

simulate :: Int -> Robot -> Position
simulate n (Robot (x, y) (xv, yv)) = ((x + (n * xv)) `mod` width, (y + (n * yv)) `mod` height)

updateQs :: Quadrants -> Position -> Quadrants
updateQs (a, b, c, d) (x, y)
  | x < mx && y < my = (a + 1, b, c, d)
  | x > mx && y < my = (a, b + 1, c, d)
  | x < mx && y > my = (a, b, c + 1, d)
  | x > mx && y > my = (a, b, c, d + 1)
  | otherwise = (a, b, c, d)
  where
    mx = width `div` 2
    my = height `div` 2

findXmasTree :: [Robot] -> Int -> Int
findXmasTree robots 0 = findXmasTree robots 1
findXmasTree robots n = if largest byX >= 10 && largest byY >= 10 then n else findXmasTree robots (n + 1)
  where
    positions = map (simulate n) robots
    byX = IM.fromListWith (++) (map (\(x, y) -> (x, [y])) positions)
    byY = IM.fromListWith (++) (map (\(x, y) -> (y, [x])) positions)
    largest = IM.foldl' (\l as -> max l $ maximum $ map length $ splitWhen (\a b -> (b - a) > 1) (sort as)) 0

part1 :: [String] -> String
part1 input = show score
  where
    robots = rights $ map (parse robot "") input
    positions = map (simulate 100) robots
    (a, b, c, d) = foldl' updateQs (0, 0, 0, 0) positions
    score = a * b * c * d

part2 :: [String] -> String
part2 input = show (findXmasTree robots 0)
  where
    robots = rights $ map (parse robot "") input
