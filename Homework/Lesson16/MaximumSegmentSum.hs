normalFind :: [Integer] -> Integer
normalFind ns = let l = length ns in
    maximum [summary ns x y | x <- [1..l], y <- [x..l]] where
        summary ns x y = sum $ take (y - x + 1) (drop (x - 1) ns)

cleverFind :: [Integer] -> Integer
cleverFind = go 0 0 where
    go s mss ns = case ns of
        []       -> mss
        (x : xs) -> go (max (s + x) 0) (max mss (s + x)) ns

scanlFind :: [Integer] -> Integer
scanlFind ns = maximum $ scanl (\s n -> max (s + n) 0) 0 ns