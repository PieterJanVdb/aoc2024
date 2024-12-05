module Utils.String where

import Data.Char (isDigit)
import Data.Text qualified as T

toInt :: String -> Int
toInt s = read [c | c <- s, isDigit c]

textToInt :: T.Text -> Int
textToInt t = toInt (T.unpack t)
