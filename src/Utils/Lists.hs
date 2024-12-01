module Utils.Lists (occurrences) where

occurrences :: (Eq a) => a -> [a] -> Int
occurrences x = length . filter (x ==)
