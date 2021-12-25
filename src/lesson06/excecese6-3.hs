isFirstHalf x xs = x `elem` firstHalf
                   where firstHalf = take middlePoint xs
                         middlePoint = (length xs) `div` 2