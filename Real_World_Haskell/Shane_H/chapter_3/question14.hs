--Using the code from the preceding three exercisesm implements Graham's scan algorithm for the convex hull of a set of 2D points.

grahamScan :: [Point] -> [Point]
grahamScan points = firstPoint : scan sortedPoints
  where
    scan (a:b:c:[])
      | isLeftTurn a b c = [b]
      | otherwise        = []
    scan (a:b:c:rest)
      | isLeftTurn a b c = b : scan (b:c:rest)
      | otherwise        = scan (a:c:rest)
    isLeftTurn a b c     = calculateDirection a b c == LeftTurn
    firstPoint           = head pointsByYValue
    pointsByYValue       = L.sortBy (O.comparing pY) $ L.sortBy (O.comparing pX) points
    sortedPoints         = firstPoint : L.sortBy (flip $ O.comparing polarAngle) (tail pointsByYValue)
      where
        polarAngle point   = angleBetween firstPoint (Point (pX point) 0) point
        angleBetween a b c = acos $ top / bottom
          where
            top                          = fromIntegral $ dotProduct ba bc
            bottom                       = magnitude ba * magnitude bc
            dotProduct (x1, y1) (x2, y2) = (x1 * x2) + (y1 * y2)
            magnitude (x, y)             = sqrt $ fromIntegral $ (x ^ 2) + (y ^ 2)
            ba                           = (pX a - pX b, pY a - pY b)
            bc                           = (pX c - pX b, pY c - pY b)
