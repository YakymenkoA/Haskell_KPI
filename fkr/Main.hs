import BinToInt (bin2int)
import Distance (distance, maxDistance)
import HList (HList (..), splitHList)
import MonadList (bindList, returnList)
import Shapes (Circle (..), HasArea (..), Square (..), Triangle (..))
import Splitter (splitBy)
import Tree (Tree (..), bfs)

main :: IO ()
main = do
  print $ bin2int "000110"

  print $ splitHList $ List [Atom 1, List [Atom 2, Atom 3], Atom 4]

  print $ splitBy "," "Hello, World!"

  let circle = Circle 5
  let triangle = Triangle 4 3
  let square = Square 10
  print $ "Circle area: " ++ show (area circle)
  print $ "Triangle area: " ++ show (area triangle)
  print $ "Square area: " ++ show (area square)

  let tree = Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 (Node 6 Empty Empty) Empty)
  print $ bfs tree

  let points = [(1, 2), (3, 4), (5, 6), (7, 8)]
  print $ maxDistance points

  let result1 = returnList 5
  print result1
  let result2 = [1, 2] >>= (\x -> [x + 1])
  print result2
