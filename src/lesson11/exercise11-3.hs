--myFoldl:: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
                        where newInit = f init x

--foldl :: (b -> a -> b) -> b -> [a] -> b


--aとｂは入れ替え可能。myFoldlとfoldlは一緒では？