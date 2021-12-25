sumSquareOrSquareSum x y = (\sumSquare squareSum ->
                             if sumSquare > squareSum
                             then sumSquare
                             else squareSum) (x^2+y^2) ((x+y)^2)
                             
inc = (\x -> x + 1)
double = (\x -> x * 2)
square = (\x -> x ^ 2)
question2_3 = (\x -> if even x
                     then x -1
                     else x * 3 - 1)
                     
simple = (\x -> x)
calcChange = (\owed given -> if given - owed > 0
                             then given - owed
                             else 0)