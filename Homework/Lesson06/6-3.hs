isPerfect :: Int -> Bool
isPerfect n = n * 2 == sum [d | d <- [1..n], mod n d == 0]

perfects :: Int -> [Int]
perfects n = [p | p <- [1..n], isPerfect p]