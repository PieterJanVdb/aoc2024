module Main where

import Day1
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
    _ -> return (Left "Not yet implemented")
