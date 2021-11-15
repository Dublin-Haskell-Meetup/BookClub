--write  a function that calculates the turn made by three two-dimenstional points and returns a Direction

tripleDirections :: [Point] -> [Direction]
tripleDirections (a:b:c:[]) = [calculateDirection a b c]
tripleDirections (a:b:c:rest) = calculateDirection a b c : tripleDirections (b:c:rest)
