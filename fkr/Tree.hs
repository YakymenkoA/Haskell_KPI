module Tree (Tree (..), bfs) where

data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

bfs :: Tree a -> [a]
bfs tree = bfsHelper [tree]

bfsHelper :: [Tree a] -> [a]
bfsHelper [] = []
bfsHelper queue =
  let (current : rest) = queue
      nextQueue = case current of
        Empty -> rest
        Node val left right -> rest ++ [left, right]
   in case current of
        Empty -> bfsHelper nextQueue
        Node val _ _ -> val : bfsHelper nextQueue
