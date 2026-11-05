module Main where
import Prelude hiding (sequenceA, sequence)

data Expr = Val Int | Add Expr Expr | Mul Expr Expr
folde :: (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
folde v a m (Val n) = v n
folde v a m (Add l r) = a (folde v a m l) (folde v a m r)
folde v a m (Mul l r) = m (folde v a m l) (folde v a m r)
eval = folde id (+) (*)
size = folde (const 1) (+) (+)

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq,Show)
instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)
newtype StateTrans a = ST (Int -> (a,Int))
app (ST f) = f
instance Functor StateTrans where
  fmap g st = ST $ \s -> let (x,s') = app st s in (g x,s')
instance Applicative StateTrans where
  pure x = ST $ \s -> (x,s)
  u <*> v = ST $ \s ->
    let (f,s') = app u s
        (x,s'') = app v s'
    in (f x,s'')
instance Monad StateTrans where
  st >>= k = ST $ \s -> let (x,s') = app st s in app (k x) s'
fresh = ST $ \n -> (n,n+1)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r
relabel t = fst (app (alabel t) 0)

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (u:us) = (:) <$> u <*> sequenceA us
sequence :: Monad m => [m a] -> m [a]
sequence [] = pure []
sequence (u:us) = do { x <- u; xs <- sequence us; pure (x:xs) }
forM xs f = sequence (map f xs)
when p u = if p then u else pure ()

main = do
  let e = Mul (Add (Val 2) (Val 3)) (Val 4)
      t = Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))
      cases =
        [ eval e == 20
        , size e == 3
        , relabel t == Node (Leaf 0) (Node (Leaf 1) (Leaf 2))
        , app (alabel t) 7 == (Node (Leaf 7) (Node (Leaf 8) (Leaf 9)),10)
        , app ((,) <$> fresh <*> fresh) 0 == ((0,1),2)
        , app (fmap (*2) fresh) 3 == (6,4)
        , sequenceA [Just 1,Just 2] == Just [1,2]
        , sequenceA [Just 1,Nothing] == (Nothing :: Maybe [Int])
        , sequenceA [[1,2],[3,4]] == [[1,3],[1,4],[2,3],[2,4]]
        , sequence [Just 1,Nothing] == (Nothing :: Maybe [Int])
        , forM [1,2] Just == Just [1,2]
        , when False Nothing == Just ()
        ]
  if and cases then putStrLn "PASS: 12 scope-13 checks" else error "Verification failed"
