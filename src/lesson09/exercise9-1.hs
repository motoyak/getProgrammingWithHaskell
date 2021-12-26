myElem _ [] = False
myElem x xs = length (filter (\y -> x == y) xs) > 0

myElem' _ [] = False
myElem' x xs = length (filter (== x) xs) > 0