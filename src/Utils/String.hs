module Utils.String where

import Data.Char (isDigit)

toInt :: String -> Int
toInt s = read [c | c <- s, isDigit c]
