and1 :: Bool -> Bool -> Bool
and1 x y | x == False = False
         | y == False = False
         | otherwise  = True

and2 :: Bool -> Bool -> Bool
and2 x y = if x == False then False else if y == False then False else True

and3 :: (Bool, Bool) -> Bool
and3 (x, y) | (x, y) == (True, True) = True
            | otherwise              = False