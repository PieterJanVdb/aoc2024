module Day9 (part1, part2) where

import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Maybe (isJust)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Utils.Lists (splitWhen)

type Disk = Vector (Maybe Int)

parse :: String -> Disk
parse = V.concat . V.toList . V.imap toBlocks . V.fromList . filter (/= '\n')
  where
    toBlocks i c = V.replicate (digitToInt c) (if even i then Just (i `div` 2) else Nothing)

tryMove :: Bool -> Disk -> Vector (Int, Maybe Int) -> Disk
tryMove shoulDefrag disk blocks = case viable of
  Nothing -> disk
  Just empty -> V.update disk (move empty blocks)
  where
    freeSpaceIdx = V.takeWhile (\x -> x < fst (V.last blocks)) $ V.elemIndices Nothing disk
    ranges = V.fromList $ splitWhen (\a b -> shoulDefrag && (b - a > 1)) $ V.toList freeSpaceIdx
    viable = if shoulDefrag then V.find (\x -> length x >= length blocks) ranges else Just (V.head ranges)
    move empty blocks =
      let blocks' = V.take (length empty) blocks
       in V.concat
            [ V.zip (V.fromList empty) (V.map snd blocks'),
              V.map (\(i, _) -> (i, Nothing :: Maybe Int)) blocks'
            ]

solve :: Bool -> Disk -> Disk
solve shoulDefrag disk = foldl' (tryMove shoulDefrag) disk moveableSpace
  where
    moveableSpace = V.groupBy (\(_, a) (_, b) -> a == b) $ V.reverse $ V.filter (isJust . snd) $ V.indexed disk

checksum :: Disk -> Int
checksum disk = V.sum $ V.catMaybes $ V.imap (\i x -> (* i) <$> x) disk

part1 :: String -> String
part1 = show . checksum . solve False . parse

part2 :: String -> String
part2 = show . checksum . solve True . parse
