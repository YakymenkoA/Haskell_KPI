{- PROBLEM 1
It is often needed to convert a number written in arabic symbols,
to a string of its textual representation, i.e. for financial documents.
Write a function intToWords that transcribes an integer into its 
textual representation in format "digit-digit-digit-...". 
-}

digitToWord :: Char -> String
digitToWord c = case c of
  '0' -> "zero"
  '1' -> "one"
  '2' -> "two"
  '3' -> "three"
  '4' -> "four"
  '5' -> "five"
  '6' -> "six"
  '7' -> "seven"
  '8' -> "eight"
  '9' -> "nine"
  _   -> error "Invalid digit"

intToWords :: (Integral a, Show a) => a -> String
intToWords x
  | x < 0     = "minus-" ++ intToWords (-x)
  | otherwise = concatMap (\c -> digitToWord c ++ "-") (init (show x)) ++ digitToWord (last (show x))

problem1 = do
  print "Problem 1"
  print $ intToWords  150  -- "one-five-zero"
  print $ intToWords    0  -- "zero"
  print $ intToWords (-10) -- "minus-one-zero"

{- PROBLEM 2
Write a function findMaxFrequency that for a given homogenous list of type a
returns a pair (a, Int) of the most frequent element (any, if there are more than one) and its frequecy. For an empty list throw an error. 
-}

findMaxFrequency :: (Ord a) => [a] -> (a, Int)
findMaxFrequency [] = error "error"
findMaxFrequency lst = foldl1 maxByFrequency [(x, count x lst) | x <- unique lst]
  where
    count x = length . filter (== x)
    unique [] = []
    unique (x:xs) = x : unique (filter (/= x) xs)
    maxByFrequency a@(_, n1) b@(_, n2) = if n1 >= n2 then a else b

problem2 = do
  print "Problem 2"
  print $ findMaxFrequency [1,2,1,3,1,4]   -- (1, 3)
  print $ findMaxFrequency [1,1,2,2]       -- (1, 2) or (2, 2)
  print $ findMaxFrequency "some sentence" -- ('e', 4)
  print $ findMaxFrequency ([] :: String)  -- error

{- PROBLEM 3
For a given system of types that represent a file system structure
write a function search that given a name returns a list of all paths
that correspond to that name.
-}

type Name = String
type Path = String
data FSNode = File Name | Dir Name [FSNode]

search :: Name -> FSNode -> [Path]
search name (File n)
  | name == n = [""]
  | otherwise = []
search name (Dir n nodes) = 
  let paths = concatMap (\node -> searchInSubtree (n ++ "/" ) node) nodes
  in if name == n then ("//" ++ n) : paths else paths
  where
    searchInSubtree :: String -> FSNode -> [Path]
    searchInSubtree prefix (File n) 
      | name == n = [prefix ++ n]
      | otherwise = []
    searchInSubtree prefix (Dir n nodes) = 
      concatMap (searchInSubtree (prefix ++ n ++ "/")) nodes

root = Dir "/" 
  [
    Dir "folder1"
    [
      File "file1",
      Dir "folder2"
      [
        File "file2",
        File "file3"
      ],
      Dir "folder3"
      [
        File "file3",
        File "file4"
      ],
      File "file5"
    ]
  ]

problem3 = do
  print $ search "file1" root  -- ["//folder1/file1"]
  print $ search "file3" root  -- ["//folder1/folder2/file3", "//folder1/folder3/file3"]
  print $ search "file4" root  -- ["//folder1/folder3/file4"]
  print $ search "file6" root  -- []

main = do
  problem1
  --problem2
  problem3
  