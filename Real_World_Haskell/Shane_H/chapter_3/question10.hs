--Using the binary tree type  that we defined earlier in this chapter, write a fuction that will determine the height of the tree. The height is the largest number of hops from the root of an Empty.

treeHeight :: Tree a => Int
treeHeight Empty   = 0
treeHeight (Node this left right) = 1 + longestRemianingPath
  where
    longestRemainingPath    = max leftHeight rightHeight
    leftHeight              = treeHeight left
    rightHeight             = treeHeight right

    
