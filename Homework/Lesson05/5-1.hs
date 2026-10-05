safe_tail :: [a] -> [a]
safe_tail (_:xs) = xs
safe_tail [] = []