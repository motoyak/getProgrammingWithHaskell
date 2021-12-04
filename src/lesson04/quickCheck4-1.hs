ifEven myFunction x = if even x
                      then myFunction x
                      else x

ifEven (\x -> x^3)