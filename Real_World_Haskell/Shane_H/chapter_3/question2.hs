-- Define a tree type that has only one constructor. Instead of the Empty constructor,  use the Maybe type to refer to node's children

data Tree a
  = Just a (Tree a) (Tree a)
  deriving (Show)
