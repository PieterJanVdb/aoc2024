module Day22 (part1, part2) where

import Data.Bits (xor)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (foldl')

evolve :: Int -> Int
evolve secret = step3
  where
    step1 = prune $ mix (secret * 64) secret
    step2 = prune $ mix (step1 `div` 32) step1
    step3 = prune $ mix (step2 * 2048) step2
    mix a secret = a `xor` secret
    prune secret = secret `mod` 16777216

sequences :: [Int] -> HashMap [Int] Int -> HashMap [Int] Int
sequences (a : b : c : d : e : xs) seqs =
  let seq = [b - a, c - b, d - c, e - d]
      seqs' = case HM.lookup seq seqs of
        Just _ -> seqs
        Nothing -> HM.insert seq e seqs
   in sequences (b : c : d : e : xs) seqs'
sequences _ seqs = seqs

findBest :: [HashMap [Int] Int] -> (Int, [Int])
findBest maps = go (head maps) (tail maps)
  where
    go master [] = maximum $ map (\(k, v) -> (v, k)) $ HM.toList master
    go master (x : xs) = go (HM.unionWith (+) master x) xs

part1 :: [String] -> String
part1 input = show $ sum $ map ((\s -> iterate evolve s !! 2000) . read) input

part2 :: [String] -> String
part2 input = show $ fst $ findBest sequenceToPriceMaps
  where
    buyerPrices = map (map (`mod` 10) . (take 2001 . iterate evolve) . read) input
    sequenceToPriceMaps = map (`sequences` HM.empty) buyerPrices
