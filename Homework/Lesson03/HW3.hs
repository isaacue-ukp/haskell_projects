module Main where

import System.Random

-- Problem #4: 猜数字小游戏
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

main :: IO ()
main = do
    n <- randomRIO (1, 100)
    putStrLn "I've got a number between 1 and 100, guess it!"
    guess n

-- 如果仅在编写程序部分，你遇到极大困难，导致难以完成，请在代码中展示你努力尝试
-- 得到的结果，并以注释形式描述你遇到的困难。如果你认真做到了这些，你的本次作业分数并不会扣减。

-- End Problem #4
