module Day3 (part1, part2) where

import Data.Maybe (catMaybes)
import Text.Parsec (choice, endOfLine, eof, many1, parse, try)
import Text.Parsec.Char (anyChar, char, digit, string)
import Text.Parsec.String (Parser)

data Instruction = Multiply Int Int | Enable | Disable deriving (Show)

mult :: Parser Instruction
mult =
  do
    _ <- string "mul("
    x <- many1 digit
    _ <- char ','
    y <- many1 digit
    _ <- char ')'
    return (Multiply (read x) (read y))

inputParser :: Parser [Instruction]
inputParser = catMaybes <$> many1 p <* eof
  where
    p =
      choice
        [ Just <$> try mult,
          Just Enable <$ try (string "do()"),
          Just Disable <$ try (string "don't()"),
          Nothing <$ anyChar
        ]

run1 :: [Instruction] -> Int
run1 = foldr op 0
  where
    op (Multiply x y) acc = acc + (x * y)
    op _ acc = acc

run2 :: [Instruction] -> Int
run2 instructions = fst $ foldr op (0, False) instructions
  where
    op Enable (acc, ignore) = (acc, False)
    op Disable (acc, ignore) = (acc, True)
    op (Multiply x y) (acc, ignore) =
      if ignore
        then (acc, ignore)
        else (acc + (x * y), ignore)

solve :: ([Instruction] -> Int) -> String -> Int
solve run input = run instructions
  where
    instructions = case parse inputParser "" input of
      Left e -> error (show e)
      Right xs -> xs

part1 :: String -> String
part1 input = show (solve run1 input)

part2 :: String -> String
part2 input = show (solve run2 input)
