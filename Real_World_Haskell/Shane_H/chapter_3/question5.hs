-- this one was interesting division can lead to a Double so both had to be cast
-- Q3 p 69 write a function that computes the mean of a list

mean :: [Int] -> Double
mean [] = 0
mean xs = total / num
  where
    total = fromIntegral (sum xs)
    num = fromIntegral (foldr (+) 1 xs)
