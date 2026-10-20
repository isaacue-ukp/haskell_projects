data Operation = A | S | M | D | E deriving(Show)

apply :: Operation -> Int -> Int -> Int
apply A x y = x + y
apply S x y = x - y
apply M x y = x * y
apply D x y = div x y
apply E x y = x ^ y

valid :: Operation -> Int -> Int -> Bool
valid A x y = x <= y
valid S x y = x > y
valid M x y = x <= y && x /= 1 && y /= 1 
valid D x y = mod x y == 0 && y /= 1
valid E x y = x /= 1 && y /= 1

data Expression = Value Int | Apply Operation Expression Expression deriving(Show)

evaluate :: Expression -> [Int]
evaluate (Value n)     = [n | n > 0]
evaluate (Apply o l r) = [apply o x y | x <- evaluate l, y <- evaluate r, valid o x y]

sublists :: [a] -> [[a]]
sublists []     = [[]]
sublists (x:xs) = map (x:) (sublists xs) ++ sublists xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

permutations :: [a] -> [[a]]
permutations []     = [[]]
permutations (x:xs) = concat $ map (interleave x) (permutations xs)

choices :: [a] -> [[a]]
choices = concat . map permutations . sublists

values :: Expression -> [Int]
values (Value n)     = [n]
values (Apply _ l r) = values l ++ values r

solution :: Expression -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && evaluate e == [n]

split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [ (x:ls, rs) | (ls,rs) <- split xs ]

type Result = (Expression, Int)

results :: [Int] -> [Result]
results []  = []
results [n] = [(Value n, n)]
results ns  = [result | (ls,rs) <- split ns, 
                       l <- results ls, r <- results rs, result <- combine l r]
    where
        combine :: Result -> Result -> [Result]
        combine (l, x) (r, y) = [(Apply o l r, apply o x y) 
                                | o <- [A, S, M, D, E], valid o x y]

lengthe :: Expression -> Int
lengthe (Value _)     = 1
lengthe (Apply _ l r) = lengthe l + lengthe r + 1

sorte :: [Expression] -> [Expression]
sorte [] = []
sorte (x:xs) = sorte ls ++ [x] ++ sorte rs
    where
        ls = [l | l <- xs, lengthe l <  lengthe x]
        rs = [r | r <- xs, lengthe x <= lengthe r]

solutions :: [Int] -> Int -> [Expression]
solutions ns n = sorte [e | (e, _) <- minPairs resultPairs]
    where
        resultPairs = [(\(a, b) -> (a, abs $ b - n)) x | ns' <- choices ns, x <- results ns'] 
        minPairs :: Ord b => [(a, b)] -> [(a, b)]
        minPairs [] = []
        minPairs [x] = [x]
        minPairs (x:xs) = case minPairs xs of
            y:ys | snd x > snd y -> y:ys
                 | snd x < snd y -> [x]
                 | otherwise     -> x:y:ys
            [] -> [x]