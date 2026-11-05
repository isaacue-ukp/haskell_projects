--Show how the Maybe type can be made foldable and
--traversable, by giving explicit definitions for
--fold, foldMap, foldr, foldl and traverse.

import Prelude hiding (Maybe (..))

data Maybe a = Nothing | Just a deriving (Eq, Show, Ord)

instance Functor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing      = Nothing
    fmap f (Just value) = Just $ f value

instance Foldable Maybe where
    foldMap :: Monoid b => (a -> b) -> Maybe a -> b
    foldMap _ Nothing      = mempty
    foldMap f (Just value) = f value

instance Traversable Maybe where
    traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
    traverse _ Nothing      = pure Nothing
    traverse f (Just value) = Just <$> (f value)