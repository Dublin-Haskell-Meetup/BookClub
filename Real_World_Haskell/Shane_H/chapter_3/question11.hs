--Consider three two-dimensional points a, b and c. If we look at the angle formed by the line segment from a to b and the line segment  from b to c, it turns left, turns right  or forms a straight line. Define a Direction data type that lets you represent these possibilities.

data Point = Point {
  pX :: Int,
  PY :: Int
  } deriving (Show)

data Direction = LeftTurn
               | StraightOn
               | Right Turn
  deriving (Show, Eq)

calculateDirection :: Post -> Point -> -> Direction
calculateDirection a b c
  | crossProductDirection > 0 = LeftTurn
  | crossProductDirection < 0 = RightTurn
  | otherwise                 = StraightOn
  where
    crossProductDirection = ((x2 - x1) * (y3 - y1)) - ((y2 - y1) * (x3 -x1))
      where
        x1     = pX a
        x2     = pX b
        x3     = pX c
        y1     = pY a
        y2     = pY b
        y3     = pY c
