module Lib
    ( qsort
    ) where

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lt ++ [x] ++ qsort rt
    where
        lt = filter (<  x) xs
        rt = filter (>= x) xs