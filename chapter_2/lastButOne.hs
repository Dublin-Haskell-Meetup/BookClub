
lastButOne  :: [a] -> a
-- type def
lastButOne [] = error "empty list"
-- handle empty list
lastButOne [x] = error "list too short"
-- handle list too short
lastButOne [x, _] = x
-- if tuple return first value, this is the desired outcome
lastButOne (x: xs) =  lastButOne xs  
-- recursively call function on tail of list until it becomes a tuple.


