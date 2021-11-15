--Define a function that joins a list of lists together using a separator value:
intersperse :: a -> [[a]] -> [a]
intersperse sep (x:xs)
  | null xs = x
  | oterwise = x ++ sep : intersperse sep xs
