followLinks :: Eq a => [(a, a)] -> Int -> a -> Maybe a
followLinks l n x 
    | n == 0    = Just x
    | n <  0    = Nothing
    | otherwise = case lookup x l of
        Nothing -> Nothing
        Just y  -> followLinks l (n-1) y

followLinks' :: Eq a => [(a, a)] -> Int -> a -> Maybe a
followLinks' l n x 
    | n == 0    = Just x
    | n <  0    = Nothing
    | otherwise = (lookup x l) >>= followLinks' l (n-1)