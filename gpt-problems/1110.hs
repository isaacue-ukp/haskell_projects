data Expression
    = Literal Integer
    | Variable String
    | Add Expression Expression
    | IfZero Expression Expression Expression
    deriving (Eq, Show)

evaluate :: [(String, Integer)] -> Expression -> Maybe Integer
evaluate _ (Literal n)          = Just n
evaluate table (Variable s)     = case [n | (s', n) <- table, s' == s] of
    []      -> Nothing
    (n : _) -> Just n
evaluate table (Add l r)        = do
    x <- evaluate table l
    y <- evaluate table r
    return (x + y)
evaluate table (IfZero is0 l r) = do
    value <- evaluate table is0
    if value == 0 
        then evaluate table l
        else evaluate table r