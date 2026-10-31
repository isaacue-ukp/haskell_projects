import Control.Applicative
import Data.Char (isAlphaNum, isSpace, isDigit)

newtype Parser a = P (String -> [(a, String)])

apply :: Parser a -> String -> [(a, String)]
apply (P f) = f

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap function parser = P $ \program -> 
        case apply parser program of
            []              -> []
            [(value, rest)] -> [(function value, rest)]
instance Applicative Parser where
    pure :: a -> Parser a
    pure value = P $ \program -> [(value, program)]

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> parser = P $ \program ->
        case apply pf program of
            []                 -> []
            [(function, rest)] -> apply (fmap function parser) rest
instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    parser >>= function = P $ \program ->
        case apply parser program of
            []              -> []
            [(value, rest)] -> apply (function value) rest
instance Alternative Parser where
    empty :: Parser a
    empty = P $ \_ -> []

    (<|>) :: Parser a -> Parser a -> Parser a
    l <|> r = P $ \program ->
        case apply l program of
            []    -> apply r program
            other -> other

item :: Parser Char
item  = P $ \program -> 
    case program of
        []     -> []
        (x:xs) -> [(x, xs)]
sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then return x else empty
digit :: Parser Char
digit  = sat isDigit
char  :: Char -> Parser Char
char x = sat (x ==)
isPrefix :: String -> Parser String
isPrefix [] = return []
isPrefix (x : xs) = do
    char x
    rest <- isPrefix xs
    return (x : rest)
nat :: Parser Int
nat = do
    xs <- some digit
    return (read xs)
space :: Parser ()
space = do
    many (sat isSpace)
    return ()
token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v
natural :: Parser Int
natural = token nat
symbol :: String -> Parser String
symbol xs = token $ isPrefix xs

expr :: Parser Int
expr  = do 
    t <- term
    let 
        a = do
            symbol "+"
            e <- expr 
            return (t + e)
        s = do
            symbol "-"
            e <- expr
            return (t - e)
    a <|> s <|> return t

term :: Parser Int
term = do 
    f <- factor
    let
        m = do
            symbol "*"
            t <- term
            return (f * t)
        d = do
            symbol "/"
            t <- term
            return (div f t)
    m <|> d <|> return f

factor :: Parser Int
factor = do
    let
        left = do
            symbol "("
            e <- expr
            symbol ")"
            return e 
    left <|> natural

eval :: String -> Maybe Int
eval xs = case apply expr xs of
    [(n, "")] -> Just n
    _         -> Nothing
