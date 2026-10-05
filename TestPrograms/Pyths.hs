isPyth :: Int -> Int -> Int -> Bool
isPyth a b c = a * a + b * b == c * c

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(a, b, c) | a <- [1..n], b <- [1..n], c <- [1..n], a * a + b * b == c * c]