import Data.Sequence.Internal.Sorting (QList (Nil))

-- Write a function that computes the number of elements in a list. To test it ensure that it gives the same answers as the standard length fucntion

myLength [] = 0
myLength (_ : xs) = 1 + myLength xs -- feels hacky

-- below I couldnt really figure it out, represent my misunderstaning of concepts in this section
-- hoping that working on solutions for the next questions will shead some light
newtype AList a = AList [a]

toAList :: [Int] -> Int
toAList = foldr (+) 1
