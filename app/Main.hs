module Main where

import Day1
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import System.Environment

main :: IO ()
main = do
  result <- run
  case result of
    Left err -> putStrLn ("Error occured: " ++ err)
    Right value -> putStrLn value

run :: IO (Either String String)
run = do
  args <- getArgs
  input <- getContents
  let day = (read (args !! 0) :: Int)
  let part = (read (args !! 1) :: Int)
  case (day, part) of
    (1, 1) -> return (Right (Day1.part1 $ lines input))
    (1, 2) -> return (Right (Day1.part2 $ lines input))
    (2, 1) -> return (Right (Day2.part1 $ lines input))
    (2, 2) -> return (Right (Day2.part2 $ lines input))
    (3, 1) -> return (Right (Day3.part1 input))
    (3, 2) -> return (Right (Day3.part2 input))
    (4, 1) -> return (Right (Day4.part1 $ lines input))
    (4, 2) -> return (Right (Day4.part2 $ lines input))
    (5, 1) -> return (Right (Day5.part1 input))
    (5, 2) -> return (Right (Day5.part2 input))
    (6, 1) -> return (Right (Day6.part1 $ lines input))
    (6, 2) -> return (Right (Day6.part2 $ lines input))
    (7, 1) -> return (Right (Day7.part1 $ lines input))
    (7, 2) -> return (Right (Day7.part2 $ lines input))
    (8, 1) -> return (Right (Day8.part1 $ lines input))
    (8, 2) -> return (Right (Day8.part2 $ lines input))
    (9, 1) -> return (Right (Day9.part1 input))
    (9, 2) -> return (Right (Day9.part2 input))
    (10, 1) -> return (Right (Day10.part1 $ lines input))
    (10, 2) -> return (Right (Day10.part2 $ lines input))
    (11, 1) -> return (Right (Day11.part1 input))
    (11, 2) -> return (Right (Day11.part2 input))
    (12, 1) -> return (Right (Day12.part1 $ lines input))
    (12, 2) -> return (Right (Day12.part2 $ lines input))
    (13, 1) -> return (Right (Day13.part1 input))
    (13, 2) -> return (Right (Day13.part2 input))
    (14, 1) -> return (Right (Day14.part1 $ lines input))
    (14, 2) -> return (Right (Day14.part2 $ lines input))
    (15, 1) -> return (Right (Day15.part1 input))
    (15, 2) -> return (Right (Day15.part2 input))
    (16, 1) -> return (Right (Day16.part1 $ lines input))
    (16, 2) -> return (Right (Day16.part2 $ lines input))
    (17, 1) -> return (Right (Day17.part1 input))
    (17, 2) -> return (Right (Day17.part2 input))
    _ -> return (Left "Not yet implemented")
