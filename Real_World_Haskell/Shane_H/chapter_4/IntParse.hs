import Data.Char (digitToInt)

asInt :: String -> Int
loop :: Int -> String -> Int
loop acc [] = acc
loop acc (x : xs) =
  let acc' = acc * 10 + digitToInt x
   in loop acc' xs
asInt xs = loop 0 xs