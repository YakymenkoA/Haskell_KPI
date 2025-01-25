module Distance (distance, maxDistance) where

import Data.List (maximum)
import Prelude hiding (maximum)

distance :: (Floating a) => (a, a) -> (a, a) -> a
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

maxDistance :: (Floating a, Eq a, Ord a) => [(a, a)] -> a
maxDistance points = maximum [distance p1 p2 | p1 <- points, p2 <- points, p1 /= p2]
