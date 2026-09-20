module Main (main) where

import System.Random 

main :: IO ()
main = do
    rnd <- randomIO :: IO Int
    print rnd
