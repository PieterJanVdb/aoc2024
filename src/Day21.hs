module Day21 (part1, part2) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List (dropWhileEnd, find, foldl', intercalate, isSubsequenceOf, nub)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as SQ
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace (trace)
import Utils.Grid (Coord, Direction (..), Grid, makeGrid, neighbours, turnLeft, turnRight)
import Utils.String (toInt)

type Cache = HashMap (Coord, Coord) [String]

ex = ["029A", "980A", "179A", "456A", "379A"]

numpad = M.filter (/= '#') $ makeGrid ["789", "456", "123", "#0A"]

dirpad = M.filter (/= '#') $ makeGrid ["#^A", "<v>"]

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

extrapolate :: Coord -> Coord -> [[Coord]] -> [String]
extrapolate from to allPaths = nub shortest
  where
    shortest = filter (\p -> length p == max) paths
    max = minimum (map length paths)
    extract a b = dropWhileEnd (/= b) . dropWhile (/= a)
    pathToSeq p = zipWith toKey p (drop 1 p)
    findPath p =
      let ps = [pathToSeq . reverse . extract to from $ p, pathToSeq . extract from to $ p]
       in find (not . null) ps
    paths = mapMaybe findPath allPaths

extrapolateAll :: Cache -> Grid Char -> String -> Coord -> [[Coord]] -> (Cache, [String])
extrapolateAll cache grid seq root allPaths =
  (\(cache', _, s) -> (cache', map moveA s)) $
    foldl' foldChar (cache, root, [[]]) seq
  where
    moveA seq = drop 1 seq <> "A"
    toCoord ch = fst $ M.findMax $ M.filter (== ch) grid
    combine l r = [x <> "A" <> y | x <- l, y <- r]
    foldChar (cache', from, seqs) ch =
      let to = toCoord ch
       in if from == to
            then (cache', from, combine seqs [[]])
            else case HM.lookup (from, to) cache' of
              Just seqs' -> (cache', to, combine seqs seqs')
              Nothing ->
                let seqs' = extrapolate from to allPaths
                 in (HM.insert (from, to) seqs' cache', to, combine seqs (extrapolate from to allPaths))

-- 0         2         9        A
-- <    A    ^  A  >  ^^   A  vvv   A
--
--
-- v<<A>>^A<A>Av
--   v  <<    A  >>   ^  A    <    A  >  A   v       A <^AA >A <vAAA >^A
-- <vA <AA >>^A vAA <^A >A <v<A >>^A vA ^A <vA     >^A <v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
part1 :: [String] -> String
part1 input = show $ sum scores
  where
    (rootN, _) = M.findMax $ M.filter (== 'A') numpad
    (rootD, _) = M.findMax $ M.filter (== 'A') dirpad
    (targetD, _) = M.findMax $ M.filter (== '<') dirpad
    numPaths = allPathsToRoot numpad 10 rootN
    dirPaths = allPathsToRoot dirpad 6 rootD
    keysPerCode = map (\c -> (c, extrapolateAll HM.empty numpad c rootN numPaths)) input
    foldSeq (cache, seqs) seq =
      let (cache', seqs') = extrapolateAll cache dirpad seq rootD dirPaths
       in (cache', seqs ++ seqs')
    seqsPerCode =
      iterate
        (map (\(c, (cache, seqs)) -> (c, foldl' foldSeq (cache, []) seqs)))
        (map (\(c, (_, seqs)) -> (c, (HM.empty, seqs))) keysPerCode)
        !! 25

    shortest = map (\(c, (_, seqs)) -> (c, minimum . map length $ seqs)) seqsPerCode
    scores = map (\(c, seq) -> toInt (init c) * seq) shortest

part2 :: [String] -> String
part2 input = undefined
