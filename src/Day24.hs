module Day24 (part1, part2) where

import Data.Bifunctor (second)
import Data.Bits ((.&.), (.^.), (.|.))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (find, intercalate, isPrefixOf, sortOn)
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S

data Op = AND | OR | XOR deriving (Show, Eq, Ord)

data Input = Gate (String, String) Op | Val Int deriving (Show, Eq, Ord)

type Outputs = HashMap String Input

parseSystem :: String -> Outputs
parseSystem puzzle = HM.union (HM.fromList (map parseVal valsIn)) (HM.fromList (map parseGate gatesIn))
  where
    (valsIn, gatesIn) = second (drop 1) $ break (== "") $ lines puzzle
    parseVal str = second (Val . read . drop 2) $ break (== ':') str
    parseGate str =
      let [l, ops, r, _, label] = words str
          op = case ops of "AND" -> AND; "OR" -> OR; "XOR" -> XOR
       in (label, Gate (l, r) op)

binToDec :: [Int] -> Int
binToDec [] = 0
binToDec (x : xs) = x + 2 * binToDec xs

decToBin :: Int -> [Int]
decToBin 0 = [0]
decToBin 1 = [1]
decToBin n
  | even n = decToBin (n `div` 2) ++ [0]
  | otherwise = decToBin (n `div` 2) ++ [1]

solve :: Outputs -> Input -> Int
solve m (Val n) = n
solve m (Gate (l, r) AND) = solve m (m HM.! l) .&. solve m (m HM.! r)
solve m (Gate (l, r) OR) = solve m (m HM.! l) .|. solve m (m HM.! r)
solve m (Gate (l, r) XOR) = solve m (m HM.! l) .^. solve m (m HM.! r)

binFor :: Outputs -> Char -> [Int]
binFor m ch = map snd $ sortOn fst $ HM.toList $ HM.map (solve m) xs
  where
    xs = HM.filterWithKey (\k _ -> [ch] `isPrefixOf` k) m

decFor :: Outputs -> Char -> Int
decFor m ch = binToDec (binFor m ch)

validNS :: String -> String -> Bool
validNS l r =
  ("x" `isPrefixOf` l && "y" `isPrefixOf` r)
    || ("y" `isPrefixOf` l && "x" `isPrefixOf` r)

findInvalidZ :: Outputs -> [(String, Input)] -> [(String, Input)]
findInvalidZ m [] = []
findInvalidZ m [x] = case x of (_, Gate _ OR) -> []; g -> [g]
findInvalidZ m (x : xs) = case x of
  (k, Gate (l, r) XOR)
    | k /= "z00" && k /= "z01" ->
        let gl = m HM.! l
            gr = m HM.! r
         in case (gl, gr) of
              (Gate _ OR, Gate _ XOR) -> findInvalidZ m xs
              (Gate _ XOR, Gate _ OR) -> findInvalidZ m xs
              (a@(Gate (la, ra) XOR), b@(Gate _ XOR)) ->
                (if validNS la ra then (r, b) else (l, a)) : findInvalidZ m xs
              (Gate _ OR, g') -> (r, g') : findInvalidZ m xs
              (g', Gate _ OR) -> (l, g') : findInvalidZ m xs
              (Gate _ XOR, g') -> (r, g') : findInvalidZ m xs
              (g', Gate _ XOR) -> (l, g') : findInvalidZ m xs
  (_, Gate (_, _) XOR) -> findInvalidZ m xs
  g -> g : findInvalidZ m xs

findInvalidW :: Outputs -> [(String, Input)] -> [(String, Input)]
findInvalidW m [] = []
findInvalidW m (x : xs) = case x of
  g@(k, Gate (l, r) XOR) ->
    if not $ validNS l r
      then g : findInvalidW m xs
      else
        if findParent k XOR && findSibling (l, r) AND
          then findInvalidW m xs
          else g : findInvalidW m xs
  g@(k, Gate (l, r) AND) | not $ validNS l r -> if findParent k OR then findInvalidW m xs else g : findInvalidW m xs
  g@(_, Gate (l, r) OR) ->
    let gl = m HM.! l
        gr = m HM.! r
     in case (gl, gr) of
          (Gate _ AND, Gate _ AND) -> findInvalidW m xs
          (Gate _ AND, x) -> (r, x) : findInvalidW m xs
          (x, Gate _ AND) -> (l, x) : findInvalidW m xs
  _ -> findInvalidW m xs
  where
    findSibling (l, r) op =
      isJust
        $ find
          ( \(_, v) -> case v of
              Gate (l', r') op -> l == l' || l == r' || r == l' || r == r'
              _ -> False
          )
        $ HM.toList m
    findParent key op =
      isJust
        $ find
          ( \(_, v) -> case v of
              Gate (l, r) op -> l == key || r == key
              _ -> False
          )
        $ HM.toList m

findInvalidWires :: Outputs -> Set (String, Input)
findInvalidWires m = S.fromList $ invalidZ ++ invalidW
  where
    gates = HM.filter (\case Gate _ _ -> True; _ -> False) m
    zs = sortOn fst $ HM.toList $ HM.filterWithKey (\k _ -> "z" `isPrefixOf` k) gates
    ws = HM.toList $ HM.filterWithKey (\k _ -> not $ "z" `isPrefixOf` k) gates
    invalidZ = findInvalidZ gates zs
    invalidW = findInvalidW gates ws

part1 :: String -> String
part1 puzzle = show $ (`decFor` 'z') $ parseSystem puzzle

part2 :: String -> String
part2 puzzle = show $ intercalate "," $ S.toList $ S.map fst invalid
  where
    system = parseSystem puzzle
    invalid = findInvalidWires system
