module HW11 where

-- Problem #1: Write a Haskell program to solve the maximum segment
--   sum problem, following the three steps in the slides.

mss :: [Integer] -> Integer
mss ns = let l = length ns in
    maximum [summary ns x y | x <- [1..l], y <- [x..l]] where
        summary ns x y = sum $ take (y - x + 1) (drop (x - 1) ns)

-- End Problem #1

-- Problem #2: Write a Haskell program to solve the maximum segment
--   sum problem, using the smart algorithm in the slides.

mss' :: [Integer] -> Integer
mss' ns = maximum $ scanl (\s n -> max (s + n) 0) 0 ns

-- End Problem #2
