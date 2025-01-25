module Splitter (splitBy) where

import Data.List (groupBy)
import Data.Char (isSpace)

splitBy :: String -> String -> [String]
splitBy delims [] = []
splitBy delims str =
    let isDelim c = c `elem` delims
        (word, rest) = break isDelim str
    in case rest of
        [] -> [word]
        (_:rest') -> word : splitBy delims rest'