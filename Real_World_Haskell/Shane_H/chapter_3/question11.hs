--Consider three two-dimensional points a, b and c. If we look at the angle formed by the line segment from a to b and the line segment  from b to c, it turns left, turns right  or forms a straight line. Define a Direction data type that lets you represent these possibilities.
module Geometry where

data Point2D = Point2D {x, y :: Double} deriving (Show, Ord, Eq)

data Direction = Left | Right | Straight deriving (Show, Eq)

turn :: Point2D -> Point2D -> Point2D -> Direction
turn p1 p2 p3
  | determinant > 0 = Geometry.Left
  | determinant < 0 = Geometry.Right
  | otherwise = Straight
  where
    determinant = (x p2 - x p1) * (y p3 - y p1) - (y p2 - y p1) * (x p3 - x p1)

turns :: [Point2D] -> [Direction]
turns (p1 : p2 : p3 : ps) = turn p1 p2 p3 : turns (p2 : p3 : ps)
turns _ = []