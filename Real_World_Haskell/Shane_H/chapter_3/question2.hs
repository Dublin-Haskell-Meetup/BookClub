-- Define a tree type that has only one constructor. Instead of the Empty constructor,  use the Maybe type to refer to node's children

data Tree a = Node a (Tree a) (Tree a)
            | Empty
  seriving(Show)
  
