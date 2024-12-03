module Main where

import Day1
import Day2
import Day3
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
    (3, 1) -> return (Right (Day3.part1 $ input))
    (3, 2) -> return (Right (Day3.part2 $ input))
    _ -> return (Left "Not yet implemented")
