module TraversableInstances where

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> ia = fmap f ia

instance Foldable Identity where
  foldr f initial (Identity a) = f a initial

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a
