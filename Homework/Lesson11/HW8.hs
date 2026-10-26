module HW8 where

adder :: IO ()
adder = do
    putStr "How many numbers? "
    n <- getLine
    let count = read n :: Int
    total <- addup count
    putStr "The total is "
    putStrLn $ show total

addup :: Int -> IO Int
addup 0 = return 0
addup n = do
    x <- getLine
    let number = read x :: Int
    others <- addup $ n - 1
    return $ others + number

-- 第二题无需提交代码