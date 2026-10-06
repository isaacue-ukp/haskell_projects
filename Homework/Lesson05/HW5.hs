module HW5 where

import Data.Char (chr, isLower, ord)

-- **=========[ Ch.05 ]=========**

-- Problem #1: define safetail
safetail :: [a] -> [a]
safetail (_:xs) = xs
safetail [] = []
-- End Problem #1

-- Problem #2: Luhn algorithm
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = mod (func a + func c + b + d) 10 == 0
func :: Int -> Int
func a = if 2 * a > 9 then 2 * a - 9 else 2 * a
-- End Problem #2

-- **=========[ Ch.06 ]=========**

-- Problem #1: Caesar crack
encode :: Int -> String -> String 
encode n xs = [ shift n x | x <- xs ] 

shift :: Int -> Char -> Char 
shift n c | isLower c  =  int2let $ mod (let2int c + n) 26
          | otherwise  =  c 

let2int :: Char -> Int 
let2int c = ord c - ord 'a' 
int2let :: Int -> Char 
int2let n = chr $ ord 'a' + n
        
table :: [Float] 
table = [ 8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 
          0.2, 0.8, 4.0, 2.4,  6.7, 7.5, 1.9, 0.1, 6.0, 
          6.3, 9.0, 2.8, 1.0,  2.4, 0.2, 2.0, 0.1 ] 

crack :: String -> String
crack xs  =  encode (-factor) xs 
    where   
        factor = position (minimum chitab) chitab      
        chitab = [ chisqr (rotate n table') table | n <- [0..25] ]
        table' = freqs xs
-- you can check : crack "aaaaab" = "eeeeef"

position :: Eq a => a -> [a] -> Int
position x xs = case targetList of
    []    -> -1
    i : _ -> i
    where targetList = [i | (x', i) <- zip xs [0..], x' == x]

rotate :: Int -> [a] -> [a]
rotate n l = snd splitedTwo ++ fst splitedTwo
    where splitedTwo = splitAt (mod n $ length l) l

freqs :: String -> [Float]
freqs cs = [100 * fromIntegral (count (int2let n) ccs) / fromIntegral (length ccs) | n <- [0..25]]
    where
        clear [] = []
        clear (x:xs) | isLower x = x : clear xs
                     | otherwise = clear xs
        ccs = clear cs
        count x xs = length [x' | x' <- xs, x == x']

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [(e - o) * (e - o) / e | (o, e) <- zip os es]
-- End Problem #1

-- Problem #2: Pythagorean triples
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(a, b, c) | a <- [1..n], b <- [1..n], c <- [1..n], a * a + b * b == c * c]
-- End Problem #2

-- Problem #3: perfect integers
perfects :: Int -> [Int]
perfects n = [p | p <- [1..n], p * 2 == sum [d | d <- [1..p], mod p d == 0]]
-- End Problem #3

-- Problem #4: scalar product
scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct xs ys = sum [x * y | (x, y) <- zip xs ys]
-- End Problem #4

-- **=========[ Ch.07 ]=========**

-- Problem #1: define prelude functions using recursions
and' :: [Bool] -> Bool
and' [] = True
and' (b:bs) = b && and' bs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (l:ls) = l ++ concat' ls

(!!!) :: [a] -> Int -> a
(!!!) (x:xs) n | n == 0    = x
               | otherwise = (!!!) xs (n - 1)

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = [x] ++ replicate' (n - 1) x

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = y == x || elem' x ys
-- End Problem #1

-- Problem #2: merge ascending lists
merge :: Ord a => [a] -> [a] -> [a]
merge [] l = l
merge (x:xs) l = merge xs ([y | y <- l, y <  x] ++ [x] ++ [y | y <- l, y >= x])
-- End Problem #2

-- Problem #3: merge sort
msort :: Ord a => [a] -> [a]
msort l | length l < 2 = l
        | otherwise    = merge (msort $ take half l) (msort $ drop half l)
    where half = div (length l) 2
-- End Problem #3