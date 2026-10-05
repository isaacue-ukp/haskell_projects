merge :: Ord a => [a] -> [a] -> [a]
merge [] l = l
merge (x:xs) l = merge xs ([y | y <- l, y <  x] ++ [x] ++ [y | y <- l, y >= x])

msort :: Ord a => [a] -> [a]
msort l | length l < 2 = l
        | otherwise    = merge (msort $ take half l) (msort $ drop half l)
    where half = div (length l) 2