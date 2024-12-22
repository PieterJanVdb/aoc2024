module Day21 (part1, part2) where

import Data.Bifunctor (second)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List (dropWhileEnd, find, foldl', group, intercalate, isSubsequenceOf, nub)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as SQ
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace (trace)
import Utils.Grid (Coord, Direction (..), Grid, makeGrid, neighbours, turnLeft, turnRight)
import Utils.String (toInt)

type Cache = HashMap (Coord, Coord) String

type FreqCount = HashMap (Coord, Coord) Int

data State = State
  { cache :: Cache,
    freq :: FreqCount,
    grid :: Grid Char,
    paths :: [[Coord]]
  }
  deriving (Show)

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

toCoord :: Grid Char -> Char -> Coord
toCoord pad ch = fst $ M.findMax $ M.filter (== ch) pad

extrapolate :: Coord -> Coord -> [[Coord]] -> String
extrapolate from to allPaths = final
  where
    final = snd $ minimum (map (\p -> (length . group $ p, p)) shortest)
    shortest = nub $ filter (\p -> length p == max) paths
    max = minimum (map length paths)
    extract a b = dropWhileEnd (/= b) . dropWhile (/= a)
    pathToSeq p = zipWith toKey p (drop 1 p)
    findPath p =
      let ps = [pathToSeq . reverse . extract to from $ p, pathToSeq . extract from to $ p]
       in find (not . null) ps
    paths = mapMaybe findPath allPaths

codeToFreq :: Grid Char -> Grid Char -> String -> Coord -> [[Coord]] -> FreqCount
codeToFreq numpad dirpad code root allPaths = snd $ foldl' foldChar (root, HM.empty) code
  where
    foldChar (from, freq) ch =
      let to = toCoord numpad ch
          seq = extrapolate from to allPaths
          incr fq x y = HM.alter (maybe (Just 1) (Just . (+ 1))) (toCoord dirpad x, toCoord dirpad y) fq
       in (to, foldl' (\fq (x, y) -> incr fq x y) freq (zip ('A' : seq) (seq ++ "A")))

extrapolateAll :: State -> Coord -> State
extrapolateAll state@(State {cache, freq, grid, paths}) root = HM.foldlWithKey' foldFreq state {freq = HM.empty} freq
  where
    foldFreq state@(State {cache, freq}) (from, to) count =
      let incr freq seq =
            foldl'
              (\fq (x, y) -> HM.alter (maybe (Just count) (Just . (+ count))) (toCoord dirpad x, toCoord dirpad y) fq)
              freq
              (if seq == "A" then [('A', 'A')] else zip ('A' : seq) (seq ++ "A"))
       in case HM.lookup (from, to) cache of
            Just seq -> state {cache, freq = incr freq seq}
            Nothing ->
              let seq = if from == to then "A" else extrapolate from to paths
               in state {cache = HM.insert (from, to) seq cache, freq = incr freq seq}

part1 :: [String] -> String
part1 input = show $ sum $ map (\(c, s) -> toInt c * toSequenceLen s) statePerCode
  where
    toDirs ((x, y), n) = ((dirpad M.! x, dirpad M.! y), n)
    (rootN, _) = M.findMax $ M.filter (== 'A') numpad
    (rootD, _) = M.findMax $ M.filter (== 'A') dirpad
    (targetD, _) = M.findMax $ M.filter (== '<') dirpad
    numPaths = allPathsToRoot numpad 10 rootN
    dirPaths = allPathsToRoot dirpad 6 rootD
    initialFreqsPerCode = map (\c -> (c, codeToFreq numpad dirpad c rootN numPaths)) input
    initialStates =
      map
        (\(c, freq) -> (c, (State {freq, cache = HM.empty, grid = dirpad, paths = dirPaths})))
        initialFreqsPerCode
    statePerCode =
      iterate
        (map (\(c, state) -> (c, extrapolateAll state rootD)))
        initialStates
        !! 2
    toSequenceLen (State {cache, freq}) = sum $ map snd $ HM.toList freq

part2 :: [String] -> String
part2 input = undefined
