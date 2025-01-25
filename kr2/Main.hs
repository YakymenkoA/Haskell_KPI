{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

class IDict d k v where
  insert       :: k -> v -> d k v -> d k v
  maybeGet     :: k -> d k v -> Maybe v
  getOrDefault :: k -> d k v -> v -> v
  contains     :: k -> d k v -> Bool
  delete       :: k -> d k v -> d k v
  elems        :: d k v -> [v]
  keys         :: d k v -> [k]
  size         :: d k v -> Int
  empty        :: d k v

data Dict k v = Dict [(k, v)]
  deriving (Eq)

instance Eq k => IDict Dict k v where
  insert :: k -> v -> Dict k v -> Dict k v
  insert key value (Dict xs) = Dict $ (key, value) : filter ((/= key) . fst) xs

  maybeGet :: k -> Dict k v -> Maybe v
  maybeGet key (Dict xs) = lookup key xs

  getOrDefault :: k -> Dict k v -> v -> v
  getOrDefault key dict def = case maybeGet key dict of
    Just value -> value
    Nothing    -> def

  contains :: k -> Dict k v -> Bool
  contains key dict = case maybeGet key dict of
    Just _  -> True
    Nothing -> False

  delete :: k -> Dict k v -> Dict k v
  delete key (Dict xs) = Dict $ filter ((/= key) . fst) xs

  elems :: Dict k v -> [v]
  elems (Dict xs) = map snd xs

  keys :: Dict k v -> [k]
  keys (Dict xs) = map fst xs

  size :: Dict k v -> Int
  size (Dict xs) = length xs

  empty :: Dict k v
  empty = Dict []

instance (Show k, Show v) => Show (Dict k v) where
  show :: Dict k v -> String
  show (Dict xs) = "{" ++ unwords (map showPair xs) ++ "}"
    where
      showPair (k, v) = show k ++ " : " ++ show v


main = do
  print $ fromPairs kvPairs
  where
    kvPairs :: [(Int, Char)]
    kvPairs = [(1,'h'), (2,'e'), (3, 'l'), (4,'l'), (5, 'o')]
    fromPairs :: [(Int, Char)] -> Dict Int Char
    fromPairs = foldl insert' empty
    insert' :: Dict Int Char -> (Int, Char) -> Dict Int Char
    insert' dict (k, v) = insert k v dict