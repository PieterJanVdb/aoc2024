module Day21 (part1, part2) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (dropWhileEnd, elemIndex, find, foldl', nub)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as SQ
import Utils.Grid (Coord, Direction (..), Grid, makeGrid, neighbours)
import Utils.String (toInt)

allPathsToRoot :: Grid Char -> Int -> Coord -> [[Coord]]
allPathsToRoot grid maxLen root = go (SQ.singleton (root, [])) []
  where
    go q0 p0 = case q0 of
      Empty -> map reverse p0
      (coord, path) :<| q1 ->
        if coord == root && (length path > 1)
          then go q1 (path : p0)
          else
            if length path > maxLen
              then go q1 p0
              else
                let maxTwice c = (\o -> length o <= 2) $ filter (== c) path
                    ns = filter maxTwice $ mapMaybe (\c -> c <$ M.lookup c grid) (neighbours coord)
                 in (`go` p0) $ foldl (\q c -> q |> (c, c : path)) q1 ns

extrapolate :: Coord -> Coord -> [[Coord]] -> [String]
extrapolate from to paths = nub $ filter (\p -> length p == max) viable
  where
    max = minimum (map length viable)
    viable = mapMaybe findPath paths
    pathToSeq p = zipWith toKey p (drop 1 p)
    findPath p =
      let ps = [pathToSeq . reverse . extract to from $ p, pathToSeq . extract from to $ p]
       in find (not . null) ps
    extract a b = dropWhileEnd (/= b) . dropWhile (/= a)
    toKey (x1, y1) (x2, y2) = case (x1 - x2, y1 - y2) of
      (0, 1) -> '<'
      (0, -1) -> '>'
      (1, 0) -> '^'
      (-1, 0) -> 'v'

extrapolateSequence :: Grid Char -> [[Coord]] -> String -> [String]
extrapolateSequence dirpad paths seq =
  map ((<> "A") . drop 1) $
    snd $
      foldl' foldChar (root, [[]]) seq
  where
    root = toCoord dirpad 'A'
    toCoord pad ch = fst $ M.findMax $ M.filter (== ch) pad
    foldChar (from, seqs) ch =
      let to = toCoord dirpad ch
          seqs' = if from == to then [[]] else extrapolate from to paths
       in (to, [x <> "A" <> y | x <- seqs, y <- seqs'])

type Cache = HashMap (String, Int) Int

shortestSequence :: Cache -> Grid Char -> [[Coord]] -> Int -> String -> (Cache, Int)
shortestSequence cache dirpad paths 0 seq = (cache, length seq)
shortestSequence cache dirpad paths depth seq = case HM.lookup (seq, depth) cache of
  Just total -> (cache, total)
  Nothing ->
    let (cache', total) = foldl' foldExtrapolations (cache, 0) extrapolations
     in (HM.insert (seq, depth) total cache', total)
  where
    subSeqs = splitSequence seq
    extrapolations = map (extrapolateSequence dirpad paths) subSeqs
    foldExtrapolations (cache, total) xs =
      let (cache', least) = foldl' foldExtrapolation (cache, maxBound) xs
       in (cache', least + total)
    foldExtrapolation (cache, least) extrapolation =
      let (cache', least') = shortestSequence cache dirpad paths (depth - 1) extrapolation
       in (cache', min least least')
    splitSequence [] = []
    splitSequence seq = case elemIndex 'A' seq of
      Just idx -> take (idx + 1) seq : splitSequence (drop (idx + 1) seq)
      Nothing -> []

solve :: [String] -> Int -> String
solve input n = show score
  where
    numpad = M.filter (/= '#') $ makeGrid ["789", "456", "123", "#0A"]
    dirpad = M.filter (/= '#') $ makeGrid ["#^A", "<v>"]
    numPaths = allPathsToRoot numpad 10 $ fst $ M.findMax $ M.filter (== 'A') numpad
    dirPaths = allPathsToRoot dirpad 6 $ fst $ M.findMax $ M.filter (== 'A') dirpad
    score = sum $ map (\c -> (toInt c *) $ mapCode c) input
    mapCode =
      (minimum . snd . foldl' foldSeq (HM.empty, []))
        . extrapolateSequence numpad numPaths
    foldSeq (cache, mins) seq =
      let (cache', min) = shortestSequence cache dirpad dirPaths n seq in (cache', min : mins)

part1 :: [String] -> String
part1 input = solve input 2

part2 :: [String] -> String
part2 input = solve input 25
