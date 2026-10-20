data Expr = Val Int | Add Expr Expr | Mul Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
folde t _ _ (Val n)   = t n
folde t f g (Add x y) = f (folde t f g x) (folde t f g y)
folde t f g (Mul x y) = g (folde t f g x) (folde t f g y)