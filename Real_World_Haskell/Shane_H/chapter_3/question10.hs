--Using the binary tree type  that we defined earlier in this chapter, write a fuction that will determine the height of the tree. The height is the largest number of hops from the root of an Empty.

data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

height :: Tree a -> Int
height Empty = 0
height (Node _ l r) = 1 + max (height l) (height r)
