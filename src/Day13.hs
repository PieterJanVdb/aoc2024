module Day13 (part1, part2) where

import Data.List (foldl')
import GHC.Float (double2Int)
import Text.Parsec (endOfLine, eof, many1, optional, parse, (<|>))
import Text.Parsec.Char (digit, string)
import Text.Parsec.String (Parser)

type Presses = (Double, Double)

presses :: Double -> Parser Presses
presses offset = solve <$> button <* endOfLine <*> button <* endOfLine <*> prize
  where
    button =
      (\x y -> (read x, read y))
        <$> (string "Button " *> (string "A: X+" <|> string "B: X+") *> many1 digit)
        <*> (string ", Y+" *> many1 digit)
    prize =
      (\x y -> (read x, read y))
        <$> (string "Prize: X=" *> many1 digit)
        <*> (string ", Y=" *> many1 digit)
    solve (a1, a2) (b1, b2) (c1, c2) =
      ( ((c1 + offset) * b2 - b1 * (c2 + offset)) / (a1 * b2 - b1 * a2),
        (a1 * (c2 + offset) - (c1 + offset) * a2) / (a1 * b2 - b1 * a2)
      )

solve :: String -> Double -> Int
solve input offset = double2Int tokens
  where
    parser = many1 (presses offset <* optional (many1 endOfLine)) <* eof
    possible = case parse parser "" input of
      Left e -> error (show e)
      Right ps -> filter (\(a, b) -> (a == fromInteger (round a)) && (b == fromInteger (round b))) ps
    tokens = foldl' (\total (a, b) -> total + (a * 3) + b) 0 possible

part1 :: String -> String
part1 input = show (solve input 0)

part2 :: String -> String
part2 input = show (solve input 10000000000000)
