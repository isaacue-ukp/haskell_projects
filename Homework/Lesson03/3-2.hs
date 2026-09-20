import Prelude hiding (div)

pdiv :: Integer -> Integer -> Integer   --div for positive integers
pdiv x y | x < y  = 0
        | x >= y = 1 + pdiv (x - y) y

div :: Integer -> Integer -> Integer
div x y | x > 0 && y > 0 = pdiv x y
        | x > 0 && y < 0 = -(pdiv x (-y))
        | x < 0 && y > 0 = -(pdiv (-x) y)
        | x < 0 && y < 0 = pdiv (-x) (-y)
-- Easy to learn that it's round toward 0.