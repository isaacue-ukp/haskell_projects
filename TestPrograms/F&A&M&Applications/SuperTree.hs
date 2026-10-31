data Tree a = Leaf a | Node [Tree a] deriving (Show)

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Leaf v)  = Leaf $ f v
    fmap f (Node vs) = Node $ map (fmap f) vs

instance Applicative Tree where
    pure :: a -> Tree a
    pure = Leaf

    (<*>) :: Tree (a -> b) -> Tree a -> Tree b
    Leaf f <*> tree  = fmap f tree
    Node fs <*> tree = Node $ map (<*> tree) fs

instance Monad Tree where
    (>>=) :: Tree a -> (a -> Tree b) -> Tree b
    Leaf v >>= f  = f v
    Node vs >>= f = Node $ fmap (>>= f) vs

data Tri a = Nod a [Tri a] deriving (Show)

instance Functor Tri where
    fmap :: (a -> b) -> Tri a -> Tri b
    fmap f (Nod v vs) = Nod (f v) $ map (fmap f) vs

instance Applicative Tri where
    pure :: a -> Tri a
    pure v = Nod v []

    (<*>) :: Tri (a -> b) -> Tri a -> Tri b
    Nod f fs <*> tree@(Nod v vs) = Nod (f v) (map (<*> tree) fs ++ map (fmap f) vs)

instance Monad Tri where
    (>>=) :: Tri a -> (a -> Tri b) -> Tri b
    Nod v vs >>= f = case f v of
        Nod w ws -> Nod w (map (>>= f) vs ++ ws)