-- Turn a list into a palindrome i.e navan
-- this one was quite easy

palindrome :: [a] -> [a]
palindrome xs = xs ++ reverse xs
