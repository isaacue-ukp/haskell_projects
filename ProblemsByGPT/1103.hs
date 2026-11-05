data Tree a = Node a [Tree a] deriving (Eq, Show)

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Node value children) = Node (f value) $ map (fmap f) children