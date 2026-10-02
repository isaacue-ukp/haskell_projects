func :: Int -> Int
func a = if 2 * a > 9 then 2 * a - 9 else 2 * a

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = if mod (func a + func c + b + d) 10 == 0 then True else False