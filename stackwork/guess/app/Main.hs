module Main (main) where

import Lib

main :: IO ()
main = do
    n <- randomRIO (1,100)
    putStrLn "I've got a number between 1 and 100, guess it!"
    guess n