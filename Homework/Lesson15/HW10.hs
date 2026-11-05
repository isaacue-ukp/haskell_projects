module HW10 where

import Prelude hiding (Maybe (..))

-- **=========[ Ch.14 ]=========**

-- Problem #1: Maybe, Foldable and Traversable
data Maybe a = Nothing | Just a
  deriving (Show, Eq, Ord)

instance Functor Maybe where
  fmap _ Nothing      = Nothing
  fmap f (Just value) = Just $ f value

instance Foldable Maybe where
  foldMap _ Nothing      = mempty
  foldMap f (Just value) = f value
  foldr _ e Nothing      = e
  foldr f e (Just value) = f value e
  foldl _ e Nothing      = e
  foldl f e (Just value) = f e value

instance Traversable Maybe where
  traverse _ Nothing      = pure Nothing
  traverse f (Just value) = fmap Just (f value)

-- End Problem #1

-- Problem #2: Tree, Foldable and Traversable
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

instance Functor Tree where
  fmap _ Leaf       = Leaf
  fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)

instance Foldable Tree where
  foldMap _ Leaf         = mempty
  foldMap f (Node l v r) = foldMap f l <> f v <> foldMap f r

  foldr _ v Leaf         = v
  foldr f u (Node l v r) = foldr f (f v (foldr f u r)) l

  foldl _ v Leaf         = v
  foldl f u (Node l v r) = foldl f (f (foldl f u l) v) r

instance Traversable Tree where
  traverse _ Leaf         = pure Leaf
  traverse f (Node l v r) = (Node) <$> traverse f l <*> f v <*> traverse f r

-- End Problem #2

-- **=========[ Ch.15 ]=========**

-- Problem #1: fibonacci using zip/tail/list-comprehension
fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x, y) <- zip fibs (drop 1 fibs)]

-- End Problem #1

-- Problem #2: Newton's square root with lazy lists
-- sqroot :: Double -> Double
-- sqroot n = head [b | (a, b) <- zip appro (tail appro), abs (a - b) < 0.00001]
--   where
--     next a = (a + n / a) / 2
--     appro = iterate next 1.0

-- End Problem #2
