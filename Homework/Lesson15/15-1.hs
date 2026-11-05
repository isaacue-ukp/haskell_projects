fib :: [Integer]
fib = fibonacci 0 1

fibonacci :: Integer -> Integer -> [Integer]
fibonacci x y = x : fibonacci y (x + y)

fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x, y) <- zip fibs (drop 1 fibs)]