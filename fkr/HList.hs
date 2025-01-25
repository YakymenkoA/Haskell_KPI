module HList (HList (..), splitHList) where

data HList a = Atom a | List [HList a]
  deriving (Show)

splitHList :: HList a -> (HList a, HList a)
splitHList (List xs) =
  let (atoms, lists) = foldr separate ([], []) xs
   in (List atoms, List lists)
  where
    separate (Atom x) (as, ls) = (Atom x : as, ls)
    separate l@(List _) (as, ls) = (as, l : ls)
splitHList atom@(Atom _) = (List [atom], List [])
