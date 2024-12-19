module Day19 (part1, part2) where

import Control.Monad (foldM)
import Control.Monad.State (MonadState, State, evalState, gets, modify)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (stripPrefix)
import Data.Maybe (mapMaybe)
import Text.Parsec (eof, many1, parse, sepBy1, sepEndBy1)
import Text.Parsec.Char (char, endOfLine, letter, spaces)
import Text.Parsec.String (Parser)

parseInput :: Parser ([String], [String])
parseInput = do
  patterns <- (many1 letter `sepBy1` (char ',' <* spaces)) <* endOfLine
  _ <- endOfLine
  towels <- many1 letter `sepEndBy1` endOfLine <* eof
  return (patterns, towels)

type Cache = HashMap String Int

fits :: [String] -> String -> State Cache Int
fits patterns [] = return 1
fits patterns design = memo design compute
  where
    compute = foldM (\a d -> (a +) <$> fits patterns d) 0 possible
    possible = mapMaybe (`stripPrefix` design) patterns

memo :: (MonadState Cache m) => String -> m Int -> m Int
memo design compute =
  gets (HM.lookup design) >>= \case
    Just r -> return r
    Nothing -> do
      r <- compute
      modify (HM.insert design r)
      return r

solve :: String -> [Int]
solve input = filter (/= 0) $ map (\d -> evalState (fits patterns d) HM.empty) designs
  where
    (patterns, designs) = case parse parseInput "input" input of
      Left e -> error (show e)
      Right r -> r

part1 :: String -> String
part1 input = show $ length $ solve input

part2 :: String -> String
part2 input = show $ sum $ solve input
