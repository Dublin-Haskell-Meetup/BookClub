--Write a function that determines whether its input list is a palindrome
-- This one took a while the Eq a was hard to find

isAPalindrome :: Eq a => [a] -> Bool
isAPalindrome xs = xs == reverse xs