import Data.Char(ord, chr, isLower)

letter :: Int -> Char
letter n = chr (ord 'a' + n)

number :: Char -> Int
number c = ord c - ord 'a'

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
    where
        shift :: Int -> Char -> Char
        shift n c | isLower c = letter (mod (number c + n) 26)
                  | otherwise = c

table :: [Float]
table = [ 8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
          0.2, 0.8, 4.0, 2.4,  6.7, 7.5, 1.9, 0.1, 6.0,
          6.3, 9.0, 2.8, 1.0,  2.4, 0.2, 2.0, 0.1 ]

frequencies :: String -> [Float]
frequencies cs = [100 * fromIntegral (count (letter n) ccs) / fromIntegral (length ccs) | n <- [0..25]]
    where
        clear [] = []
        clear (x:xs) | isLower x = x : clear xs
                     | otherwise = clear xs
        ccs = clear cs
        count x xs = length [x' | x' <- xs, x == x']

chiSquare :: [Float] -> [Float] -> Float
chiSquare os es = sum [(e - o) * (e - o) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n l = snd splitedTwo ++ fst splitedTwo
    where splitedTwo = splitAt (mod n $ length l) l

position :: Eq a => a -> [a] -> Int
position x xs = case targetList of
    []    -> -1
    i : _ -> i
    where targetList = [i | (x', i) <- zip xs [0..], x' == x]

crack :: String -> String
crack xs = encode (- delta) xs
    where
        delta = position (minimum chiTable) chiTable
        chiTable = [chiSquare (rotate n $ frequencies xs) table | n <- [0..25]]