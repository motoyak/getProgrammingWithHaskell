ifEven myFunction x = if even x
                      then myFunction x
                      else x

getIfXEven f = (\x -> ifEven f x)

ifEvenInc = getIfXEven (\x -> x + 1)
ifEvenDouble = getIfXEven (\x -> x * 2)
ifEvenSquare = getIfXEven (\x -> x^2)