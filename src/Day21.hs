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

toKey :: Coord -> Coord -> Char
toKey (x1, y1) (x2, y2) = case (x1 - x2, y1 - y2) of
  (0, 1) -> '<'
  (0, -1) -> '>'
  (1, 0) -> '^'
  (-1, 0) -> 'v'

toCoord :: Grid Char -> Char -> Coord
toCoord pad ch = fst $ M.findMax $ M.filter (== ch) pad

extrapolate :: Coord -> Coord -> [[Coord]] -> [String]
extrapolate from to allPaths = shortest
  where
    shortest = nub $ filter (\p -> length p == max) paths
    max = minimum (map length paths)
    extract a b = dropWhileEnd (/= b) . dropWhile (/= a)
    pathToSeq p = zipWith toKey p (drop 1 p)
    findPath p =
      let ps = [pathToSeq . reverse . extract to from $ p, pathToSeq . extract from to $ p]
       in find (not . null) ps
    paths = mapMaybe findPath allPaths

codeToSequences :: Grid Char -> Grid Char -> String -> Coord -> [[Coord]] -> [String]
codeToSequences numpad dirpad code root allPaths =
  map ((<> "A") . drop 1) $
    snd $
      foldl' foldChar (root, [[]]) code
  where
    combine l r = [x <> "A" <> y | x <- l, y <- r]
    foldChar (from, seqs) ch =
      let to = toCoord numpad ch
       in (to, combine seqs $ extrapolate from to allPaths)

extrapolateSequence :: Grid Char -> [[Coord]] -> String -> [String]
extrapolateSequence dirpad paths seq =
  map ((<> "A") . drop 1) $
    snd $
      foldl' foldChar (root, [[]]) seq
  where
    root = fst $ M.findMax $ M.filter (== 'A') dirpad
    combine l r = [x <> "A" <> y | x <- l, y <- r]
    foldChar (from, seqs) ch =
      let to = toCoord dirpad ch
       in if from == to
            then (from, combine seqs [[]])
            else
              let seqs' = extrapolate from to paths
               in (to, combine seqs seqs')

splitSequence :: String -> [String]
splitSequence [] = []
splitSequence seq = case elemIndex 'A' seq of
  Just idx -> take (idx + 1) seq : splitSequence (drop (idx + 1) seq)
  Nothing -> []

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
    foldExtrapolations (cache', total) xs =
      let least = foldl' foldExtrapolation (cache', maxBound) xs
       in (fst least, snd least + total)
    foldExtrapolation (cache, least) extrapolation =
      let (cache', least') = shortestSequence cache dirpad paths (depth - 1) extrapolation
       in (cache', min least least')

solve :: [String] -> Int -> String
solve input n = show score
  where
    numpad = M.filter (/= '#') $ makeGrid ["789", "456", "123", "#0A"]
    dirpad = M.filter (/= '#') $ makeGrid ["#^A", "<v>"]
    (rootN, _) = M.findMax $ M.filter (== 'A') numpad
    (rootD, _) = M.findMax $ M.filter (== 'A') dirpad
    (targetD, _) = M.findMax $ M.filter (== '<') dirpad
    numPaths = allPathsToRoot numpad 10 rootN
    dirPaths = allPathsToRoot dirpad 6 rootD
    seqsPerCode = map (\c -> (c, codeToSequences numpad dirpad c rootN numPaths)) input
    score =
      sum $
        map (\(c, seqs) -> (toInt c *) $ minimum $ snd $ foldl' foldSeq (HM.empty, []) seqs) seqsPerCode
    foldSeq (cache, mins) seq =
      let (cache', min) = shortestSequence cache dirpad dirPaths n seq in (cache', min : mins)

part1 :: [String] -> String
part1 input = solve input 2

part2 :: [String] -> String
part2 input = solve input 25
