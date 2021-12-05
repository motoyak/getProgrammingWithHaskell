ifEven myFunction x = if even x
                      then myFunction x
                      else x

genIfXEven f = (\x -> ifEven f x)