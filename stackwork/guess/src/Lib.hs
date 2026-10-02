module Lib
    ( guess
    ) where

guess :: Integer -> IO ()
guess target = do
    putStrLn "Input your guess here:"
    n <- getLine
    let num = (read n :: Integer)
    if num /= target
        then do
            if num < target then putStrLn "Emmm, a bit smaller. Try again!"
            else putStrLn "Not so big. Try again!"
            guess target
        else putStrLn "Bingo! You've got it."