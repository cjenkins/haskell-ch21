module Lib where

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

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure b = Constant mempty
  (Constant a) <*> cab = Constant a

instance Foldable (Constant a) where
  foldr f initial (Constant a) = initial

instance Traversable (Constant a) where
  traverse f (Constant a) = Constant <$> pure a

data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap f Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Applicative Optional where
  pure = Yep
  Nada <*> _ = Nada
  _ <*> Nada = Nada
  Yep f <*> ya = fmap f ya

instance Foldable Optional where
  foldr f initial Nada = initial
  foldr f initial (Yep a) = f a initial

instance Traversable Optional where
  traverse f Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a
