--Define a function that joins a list of lists together using a separator value:
-- not sure about this one might come back to it

intersperse :: a -> [[a]] -> [a]
intersperse x xs = Op [] xs
  where
    Op ops [] = ops
    Op ops (a : b) = Op (ops ++ h ++ [x]) b
