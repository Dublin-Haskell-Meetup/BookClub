--Create a function that sorts a list of lists based on the length of each sublist.

-- Alot of these seem to come down to knowledge of libraries and functions
-- I think it makes sense to google solutions to avoid reinventing the wheel on these

import Data.List

sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy compareList
  where
    compareList x y = compare (length x) (length y)
