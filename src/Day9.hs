module Day9 (part1, part2) where

import Data.Char (digitToInt)
import Data.List (find, foldl')
import Data.Maybe (isJust)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Utils.Lists (splitWhen)

type Disk = Vector (Maybe Int)

data Move = Single | Whole

parse :: String -> Disk
parse = V.concat . V.toList . V.imap toBlocks . V.fromList . filter (/= '\n')
  where
    toBlocks i c = V.replicate (digitToInt c) (if even i then Just (i `div` 2) else Nothing)

tryMove :: Move -> Disk -> Vector (Int, Maybe Int) -> Disk
tryMove moveKind disk used = case viable of
  Nothing -> disk
  Just empty -> V.update disk (generateMoves empty used)
  where
    emptyIdx = V.takeWhile (\x -> x < fst (V.last used)) $ V.elemIndices Nothing disk
    viable =
      case moveKind of
        Single -> Just (V.toList emptyIdx)
        Whole ->
          find (\x -> length x >= length used) $
            splitWhen (\a b -> b - a > 1) $
              V.toList emptyIdx
    generateMoves empty used =
      let used' = V.take (length empty) used
       in V.concat
            [ V.zip (V.fromList empty) (V.map snd used'),
              V.map (\(i, _) -> (i, Nothing :: Maybe Int)) used'
            ]

solve :: Move -> Disk -> Disk
solve shoulDefrag disk = foldl' (tryMove shoulDefrag) disk moveableSpace
  where
    moveableSpace =
      V.groupBy (\(_, a) (_, b) -> a == b) $
        V.reverse $
          V.filter (isJust . snd) $
            V.indexed disk

checksum :: Disk -> Int
checksum disk = V.sum $ V.catMaybes $ V.imap (\i x -> (* i) <$> x) disk

part1 :: String -> String
part1 = show . checksum . solve Single . parse

part2 :: String -> String
part2 = show . checksum . solve Whole . parse
