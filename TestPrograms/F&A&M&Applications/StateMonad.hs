type State = Int
newtype StateTrans a = ST (State -> (a, State))

apply :: StateTrans a -> State -> (a, State)
apply (ST f) = f

instance Functor StateTrans where
    fmap :: (a -> b) -> StateTrans a -> StateTrans b
    fmap function st = ST (\state -> 
        let (result, state') = apply st state 
        in (function result, state')
        )

instance Applicative StateTrans where
    pure :: a -> StateTrans a
    pure x = ST (\state -> (x, state))

    (<*>) :: StateTrans (a -> b) -> StateTrans a -> StateTrans b
    stf <*> st = ST (\state ->
        let (function, state') = apply stf state
            (result, state'') = apply st state'
        in (function result, state'')
        )

instance Monad StateTrans where
    (>>=) :: StateTrans a -> (a -> StateTrans b) -> StateTrans b
    st >>= function = ST (\state -> 
        let (result, state') = apply st state 
        in apply (function result) state'
        )


data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

fresh :: StateTrans Int
fresh = ST (\state -> (state, state + 1))

alabel :: Tree a -> StateTrans (Tree Int)
alabel (Leaf _)   = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

relabel :: Tree a -> Tree Int
relabel tree = fst $ apply (alabel tree) 0

mlabel1 :: Tree a -> StateTrans (Tree Int)
mlabel1 (Leaf _)   = fresh >>= \n -> pure $ Leaf n
mlabel1 (Node l r) =
    mlabel1 l >>= \x -> 
    mlabel1 r >>= \y -> pure $ Node x y


mlabel2 :: Tree a -> StateTrans (Tree Int)
mlabel2 (Leaf _)   = do
    n <- fresh
    pure $ Leaf n
mlabel2 (Node l r) = do
    x <- mlabel2 l
    y <- mlabel2 r
    pure $ Node x y

relabel1 :: Tree a -> Tree Int
relabel1 tree = fst $ apply (mlabel1 tree) 0

relabel2 :: Tree a -> Tree Int
relabel2 tree = fst $ apply (mlabel2 tree) 0