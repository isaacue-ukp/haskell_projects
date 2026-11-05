module Main where

import Control.Applicative
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (ap, unless)
import Data.Char
import Data.Foldable (fold)
import Data.Monoid
import System.Exit (exitFailure)

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show)
instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)
instance Foldable Tree where
  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = foldMap f l <> foldMap f r
instance Traversable Tree where
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l r) = Node <$> traverse f l <*> traverse f r

type State = Int
newtype StateTrans a = ST (State -> (a,State))
app :: StateTrans a -> State -> (a,State)
app (ST f) = f
instance Functor StateTrans where
  fmap f st = ST $ \s -> let (x,s') = app st s in (f x,s')
instance Applicative StateTrans where
  pure x = ST $ \s -> (x,s)
  (<*>) = ap
instance Monad StateTrans where
  st >>= k = ST $ \s -> let (x,s') = app st s in app (k x) s'
fresh :: StateTrans Int
fresh = ST $ \n -> (n,n+1)

newtype Parser a = P (String -> [(a,String)])
run :: Parser a -> String -> [(a,String)]
run (P f) = f
instance Functor Parser where
  fmap g p = P $ \s -> case run p s of
    [] -> []
    [(x,out)] -> [(g x,out)]
instance Applicative Parser where
  pure x = P $ \s -> [(x,s)]
  (<*>) = ap
instance Monad Parser where
  p >>= k = P $ \s -> case run p s of
    [] -> []
    [(x,out)] -> run (k x) out
instance Alternative Parser where
  empty = P $ \_ -> []
  p <|> q = P $ \s -> case run p s of
    [] -> run q s
    rs -> rs
  many p = some p <|> pure []
  some p = (:) <$> p <*> many p
item :: Parser Char
item = P $ \s -> case s of { [] -> []; (x:xs) -> [(x,xs)] }
sat :: (Char -> Bool) -> Parser Char
sat p = do { x <- item; if p x then pure x else empty }
char :: Char -> Parser Char
char c = sat (== c)
nat :: Parser Int
nat = read <$> some (sat isDigit)
space :: Parser ()
space = many (sat isSpace) >> pure ()
token :: Parser a -> Parser a
token p = do { space; x <- p; space; pure x }
symbol :: String -> Parser String
symbol s = token (string s)
string :: String -> Parser String
string [] = pure []
string (x:xs) = (:) <$> char x <*> string xs
expr, term, factor :: Parser Int
expr = do { t <- term;
  (do { symbol "+"; e <- expr; pure (t+e) }) <|> pure t }
term = do { f <- factor;
  (do { symbol "*"; t <- term; pure (f*t) }) <|> pure f }
factor = (do { symbol "("; e <- expr; symbol ")"; pure e }) <|> token nat

foldN :: (a -> a) -> a -> Int -> a
foldN h z 0 = z
foldN h z n = h (foldN h z (n-1))
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = let ys = subs xs in ys ++ map (x:) ys
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concatMap (interleave x) (perms xs)
choices :: [a] -> [[a]]
choices = concatMap perms . subs
fibs :: [Integer]
fibs = 0:1:zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
  let checks =
        [ ("fold directions", foldr (-) 0 [1,2,3] == 2 && foldl (-) 0 [1,2,3] == -6)
        , ("reverse", foldl (flip (:)) [] [1,2,3] == [3,2,1])
        , ("filter fold", foldr (\x ys -> if even x then x:ys else ys) [] [1..6] == [2,4,6])
        , ("negative integral division", (-5) `div` 2 == -3 && (-5) `mod` 2 == 1)
        , ("natural fold", foldN (+2) 1 3 == 7)
        , ("choices", length (choices [1,2,3]) == 16)
        , ("tree fold order", fold (Node (Leaf [1]) (Leaf [2,3])) == [1,2,3])
        , ("tree traverse", traverse Just (Node (Leaf 1) (Leaf 2)) == Just (Node (Leaf 1) (Leaf 2)))
        , ("tree failure", traverse (\n -> if n>0 then Just n else Nothing) (Node (Leaf 1) (Leaf 0)) == Nothing)
        , ("state sequencing", app (do { x <- fresh; y <- fresh; pure (x,y) }) 0 == ((0,1),2))
        , ("reader bind", (((+1) >>= \x r -> x+r) 3 :: Int) == 7)
        , ("list applicative", ((*) <$> [1,2] <*> [3,4]) == [3,4,6,8])
        , ("applicative interchange", ([(*2),(+3)] <*> pure 4) == (pure ($ 4) <*> [(*2),(+3)]))
        , ("applicative composition", (pure (.) <*> [(*2)] <*> [(+3)] <*> [4,5]) == ([(*2)] <*> ([(+3)] <*> [4,5])))
        , ("parser pure", run (pure 1) "abc" == [(1,"abc")])
        , ("parser rollback", run (string "ax" <|> string "ab") "abc" == [("ab","c")])
        , ("parser many", run (many (char 'a')) "aab" == [("aa","b")])
        , ("parser empty many", run (many (char 'a')) "b" == [("","b")])
        , ("parser some failure", run (some (char 'a')) "b" == [])
        , ("parser precedence", run expr "2 * 3 + 4" == [(10,"")])
        , ("parser parentheses", run expr " 2 * (3 + 4) " == [(14,"")])
        , ("parser remaining input", run expr "2+" == [(2,"+")])
        , ("sequenceA success", sequenceA [Just 1,Just 2] == Just [1,2])
        , ("sequenceA failure", sequenceA [Just 1,Nothing] == (Nothing :: Maybe [Int]))
        , ("monoids", getSum (mconcat [Sum 2,Sum 3]) == 5 && getAll (mempty :: All))
        , ("infinite fibs", take 8 fibs == [0,1,1,2,3,5,8,13])
        , ("foldr short circuit", foldr (&&) True (False:repeat True) == False)
        , ("lazy ignores undefined", const 1 undefined == (1::Int))
        , ("WHNF only", (const 1 $! [undefined]) == (1::Int))
        ]
  mapM_ (\(name,ok) -> unless ok (putStrLn ("FAIL: "++name))) checks
  strictResult <- try (evaluate (const (1::Int) $! undefined)) :: IO (Either SomeException Int)
  let strictPass = case strictResult of { Left _ -> True; Right _ -> False }
  unless (all snd checks && strictPass) exitFailure
  putStrLn ("PASS: " ++ show (length checks + 1) ++ " reference checks")
