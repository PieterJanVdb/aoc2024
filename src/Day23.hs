module Day23 (part1, part2) where

import Data.Bifunctor (first, second)
import Data.List (foldl', intercalate, intersect, isPrefixOf, nub, singleton, sort, subsequences)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple qualified as T
import Debug.Trace (trace)
import Utils.Lists (pairs)

parse :: [String] -> [(String, String)]
parse = map (second (drop 1) . break (== '-'))

connsPerPC :: [(String, String)] -> Map String (Set String)
connsPerPC conns =
  M.unionsWith S.union $
    map (M.fromListWith S.union . prep) [conns, map T.swap conns]
  where
    prep = map (second S.singleton)

isValidSet :: Map String (Set String) -> String -> (String, String) -> Bool
isValidSet conns tpc (apc, bpc) = check apc bpc && check bpc apc
  where
    check a b = case M.lookup a conns of
      Just pcs -> let sub = S.fromList [tpc, b] in S.intersection sub pcs == sub
      Nothing -> False

validSets :: Map String (Set String) -> [[String]]
validSets conns = nub $ map sort $ concatMap (uncurry mapTConn) $ M.toList tConns
  where
    tConns = M.filterWithKey (\k _ -> "t" `isPrefixOf` k) conns
    mapTConn tpc pcs =
      map (\(a, b) -> [tpc, a, b]) $
        filter (isValidSet conns tpc) $
          pairs (S.toList pcs)

intersections :: [Set String] -> Set String
intersections [] = S.empty
intersections (set : sets) = go set sets
  where
    go master [] = master
    go master (x : xs) = go (S.intersection master x) xs

biggestChain :: [Set String] -> Set String
biggestChain xss = snd $ maximum $ mapMaybe (\sets -> withSize sets $ intersections sets) subs
  where
    withSize sets i = if S.size i == length sets then Just (S.size i, i) else Nothing
    subs = filter ((>= 2) . length) $ subsequences xss

connectedSets :: Map String (Set String) -> Set String -> [Set String]
connectedSets conns set = mapMaybe (\s -> S.insert s <$> M.lookup s conns) (S.toList set)

part1 :: [String] -> String
part1 input = show $ length $ validSets $ connsPerPC $ parse input

part2 :: [String] -> String
part2 input = show $ intercalate "," $ S.toList biggest
  where
    conns = connsPerPC $ parse input
    biggest = snd $ maximum $ map (\c -> (S.size c, c)) $ (\m -> foldl' (findChain m) [] (M.toList m)) conns
    findChain conns chains (pc, pcs) =
      if any (S.member pc) chains
        then chains
        else biggestChain (S.insert pc pcs : connectedSets conns pcs) : chains
