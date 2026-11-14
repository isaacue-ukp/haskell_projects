collectResults :: [Maybe a] -> Maybe [a]
collectResults []                    = Just []
collectResults (Nothing : _)         = Nothing
collectResults (Just value : others) = case collectResults others of
    Nothing     -> Nothing
    Just values -> Just (value : values)
--  [] is just an empty list, while [Nothing] is a list 
--  with an element "Nothing", which will destroy other 
--  elements when they come in.

--  another one using fmap
collectResults' :: [Maybe a] -> Maybe [a]
collectResults' []                    = Just []
collectResults' (Nothing : _)         = Nothing
collectResults' (Just value : others) = fmap (value:) $ collectResults' others