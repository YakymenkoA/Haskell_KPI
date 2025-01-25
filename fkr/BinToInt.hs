module BinToInt (bin2int) where

import Data.Char (digitToInt)

bin2int :: String -> Int
bin2int binStr = foldl (\acc x -> acc * 2 + digitToInt x) 0 binStr