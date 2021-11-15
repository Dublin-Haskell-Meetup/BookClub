-- Write the converse of fromList for the List type: a function that takes a List from a and generates [a]

-- Below Cons is just a constructor and replaces :, recursive call of to list
toList (Cons x xs) = x : toList xs
-- Using nill to handle empty case
toList Nil = []
