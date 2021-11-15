Mean :: [Int] -> Double

Mean l = total/fromIntegral (length l)
  where total = sum $ map fromIntegral l
