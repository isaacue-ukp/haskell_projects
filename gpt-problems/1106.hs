data Tree a = Node a [Tree a] deriving (Eq, Show)

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Node value children) = Node (f value) $ map (fmap f) children

pruneTree :: (a -> Bool) -> Tree a -> Maybe (Tree a)
pruneTree is (Node value children) = case is value of
    False -> Nothing
    True  -> Just (Node value $ foldr filterTree [] children) where
        filterTree t ts = case pruneTree is t of
            Nothing -> ts
            Just t' -> t' : ts