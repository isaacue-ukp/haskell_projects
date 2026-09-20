module Lib
    ( guess
    ) where

guess :: String -> IO ()
guess target = do
    putStrLn "Input your guess here:"
    n <- getLine
    if n /= target
        then do
            if n < target then putStrLn "Emmm, a bit smaller. Try again!"
            else putStrLn "Not so big. Try again!"
            guess target
        else putStrLn "Bingo! You've got it."