--In a similar manner, show how the following type of binary trees
--with data in their nodes can be made into a foldable and
--traversable type

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap _ Leaf       = Leaf
    fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)


instance Foldable Tree where
    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap _ Leaf         = mempty
    foldMap f (Node l v r) = foldMap f l <> f v <> foldMap f r

    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr _ v Leaf         = v
    foldr f u (Node l v r) = foldr f (f v (foldr f u r)) l

    foldl :: (b -> a -> b) -> b -> Tree a -> b
    foldl _ v Leaf         = v
    foldl f u (Node l v r) = foldl f (f (foldl f u l) v) r

instance Traversable Tree where
    traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse _ Leaf         = pure Leaf
    traverse f (Node l v r) = (Node) <$> traverse f l <*> f v <*> traverse f r