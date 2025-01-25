module Shapes (Circle (..), Triangle (..), Square (..), HasArea (..)) where

data Circle = Circle {radius :: Double} deriving (Show)

data Triangle = Triangle {base :: Double, height :: Double} deriving (Show)

data Square = Square {side :: Double} deriving (Show)

class HasArea a where
  area :: a -> Double

instance HasArea Circle where
  area (Circle r) = pi * r * r

instance HasArea Triangle where
  area (Triangle b h) = 0.5 * b * h

instance HasArea Square where
  area (Square s) = s * s
