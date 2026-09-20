module Main (main) where

import Lib

main :: IO ()
main = do
    print (qsort[1, 1, 4, 5, 1, 4])
    print (qsort[1, 9, 1, 9, 8, 1, 0])
    print (qsort "mastcell")
    print (qsort "massivecell")
    print (qsort "haskell")
    print (qsort "Isaacue")