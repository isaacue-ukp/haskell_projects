import Text.Read (readMaybe)

readNumber :: Int -> IO (Maybe Int)
readNumber attempts
    | attempts <= 0 = pure Nothing
    | otherwise     = do
        input <- getLine
        case readMaybe input of
            Just number -> pure (Just number)
            Nothing -> readNumber (attempts - 1)