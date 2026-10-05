merge :: Ord a => [a] -> [a] -> [a]
merge [] l = l
merge (x:xs) l = merge xs ([y | y <- l, y <  x] ++ [x] ++ [y | y <- l, y >= x])