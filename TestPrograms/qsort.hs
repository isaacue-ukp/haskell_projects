qsort [] = []
qsort (x:xs) = qsort frt ++ [x] ++ qsort bck
    where
        frt = [a | a <- xs, a <= x]
        bck = [a | a <- xs, a > x]