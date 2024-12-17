module Utils.Lists (occurrences, pairs, splitWhen, chunks, (!?)) where

import Data.List (tails, unfoldr)

occurrences :: (Eq a) => a -> [a] -> Int
occurrences x = length . filter (x ==)

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)

splitWhen :: (a -> a -> Bool) -> [a] -> [[a]]
splitWhen f = foldr go [[]]
  where
    go x acc = (x : xs) : xss
      where
        xs : xss = case acc of
          (z : _) : _ | f x z -> [] : acc
          _ -> acc

-- https://hackage.haskell.org/package/ghc-internal-9.1001.0/docs/src/GHC.Internal.List.html#%21%3F
(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0 = Nothing
  | otherwise =
      foldr
        ( \x r k -> case k of
            0 -> Just x
            _ -> r (k - 1)
        )
        (const Nothing)
        xs
        n
