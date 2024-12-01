module Utils.Lists (alternate, occurrences) where

alternate :: [a] -> ([a], [a])
alternate l = alternate' l [] []

alternate' :: [a] -> [a] -> [a] -> ([a], [a])
alternate' [] a b = (reverse a, reverse b)
alternate' [_] a b = (reverse a, reverse b)
alternate' (x : y : xs) a b = alternate' xs (x : a) (y : b)

occurrences :: (Eq a) => a -> [a] -> Int
occurrences x = length . filter (x ==)
