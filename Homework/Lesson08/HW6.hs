module HW6 where

import Data.Char (chr)

-- Ch.08

-- Problem #1: desugar list comprehension using map and filter
theExpr :: (a -> Bool) -> (a -> b) -> [a] -> [b]
-- theExpr p f xs = [f x | x <- xs, p x]
theExpr p f xs = map f $ filter p xs 
-- End Problem #1

-- Problem #2: redefine map/filter with foldr
filter_ :: (a -> Bool) -> [a] -> [a]
filter_ p = foldr (\x l -> if p x then x : l else l) []

map_ :: (a -> b) -> [a] -> [b]
map_ f = foldr (\x l -> f x : l) []
-- End Problem #2

-- Problem #3: error checking for binary string transmitter
type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

decode :: [Bit] -> String
-- modify this line to add error checking
decode = map (chr . bin2int) . dropcheck . chop

dropcheck :: [[Bit]] -> [[Bit]]
dropcheck [] = []
dropcheck (x:xs) | length x == 9 && last x == checkBit (init x) = init x : dropcheck xs
                 | otherwise                                    = error " "
    where checkBit l = if odd $ length $ filter (==1) l then 1 else 0

chop :: [Bit] -> [[Bit]]
chop [] = []
chop bits = take 9 bits : chop (drop 9 bits) -- hint: not 'chop8' any more
-- you can check: decode [1,0,0,0,0,1,1,0,1] == "a"

-- End Problem #3
