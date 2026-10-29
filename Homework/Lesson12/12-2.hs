data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
    fmap :: (a -> b) -> Expr a -> Expr b
    fmap f (Var x)   = Var (f x)
    fmap _ (Val n)   = Val n
    fmap f (Add l r) = Add (fmap f l) (fmap f r)

instance Applicative Expr where
    pure :: a -> Expr a
    pure = Var

    (<*>) :: Expr (a -> b) -> Expr a -> Expr b
    Var f   <*> x = fmap f x
    Val n   <*> _ = Val n
    Add f g <*> x = Add (f <*> x) (g <*> x)

instance Monad Expr where
    (>>=) :: Expr a -> (a -> Expr b) -> Expr b
    Var x   >>= f = f x
    Val n   >>= f = Val n
    Add l r >>= f = Add (l >>= f) (r >>= f)