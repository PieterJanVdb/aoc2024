module Utils.Lists (occurrences, (!?)) where

occurrences :: (Eq a) => a -> [a] -> Int
occurrences x = length . filter (x ==)

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
