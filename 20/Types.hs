module Types (Direction(..), Element(..), Point) where

data Direction = N | E | S | W deriving (Show, Eq, Ord)

data Element = Directions [Direction] |
               Sequence [Element] |
               Branch [Element] deriving (Show, Eq, Ord)

type Point = (Int, Int)
