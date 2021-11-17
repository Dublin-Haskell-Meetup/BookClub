-- Write your own "safe" definitions of the standard partial list functions, but make sure they never fail.

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safeHead' :: [a] -> Maybe a
safeHead' [] = Nothing
safeHead' xs = Just (head xs)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_ : xs) = Just xs

safeTail' :: [a] -> Maybe [a]
safeTail' [] = Nothing
safeTail' xs = Just (tail xs)

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_ : xs) = safeLast xs

safeLast' :: [a] -> Maybe a
safeLast' xs = safeHead (reverse xs)

safeLast'' :: [a] -> Maybe a
safeLast'' = safeHead . reverse

safeLast''' :: [a] -> Maybe a
safeLast''' [] = Nothing
safeLast''' xs = Just (last xs)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)