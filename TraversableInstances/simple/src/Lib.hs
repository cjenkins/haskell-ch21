module Lib where

import Data.Monoid

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

data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Foldable List where
  foldr f initial Nil = initial
  foldr f initial (Cons a l) = f a $ foldr f initial l

instance Traversable List where
  traverse f Nil = pure Nil
  traverse f (Cons a l) = Cons <$> (f a) <*> (traverse f l)

data Three a b c = Three a b c deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldr f initial (Three a b c) = f c initial

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

data Pair a b = Pair a b deriving (Eq, Ord, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldr f initial (Pair a b) = f b initial

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

data Big a b = Big a b b deriving (Eq, Ord, Show)

instance Functor (Big a) where
  fmap f (Big a b1 b2) = Big a (f b1) (f b2)

instance Foldable (Big a) where
  foldMap f (Big a b1 b2) = mappend (f b1) (f b2)

instance Traversable (Big a) where
  traverse f (Big a b1 b2) = Big a <$> f b1 <*> f b2

data Bigger a b = Bigger a b b b deriving (Eq, Ord, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b1 b2 b3) = Bigger a (f b1) (f b2) (f b3)

instance Foldable (Bigger a) where
  foldMap f (Bigger a b1 b2 b3) = f b1 <> f b2 <> f b3

instance Traversable (Bigger a) where
  traverse f (Bigger a b1 b2 b3) = Bigger a <$> f b1 <*> f b2 <*> f b3

data Tree a =
  Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node treeLeft a treeRight) = Node (f <$> treeLeft) (f a) (f <$> treeRight)
  
-- foldMap is a bit easier
-- and looks more natural,
-- but you can do foldr too
-- for extra credit.
instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node treeLeft a treeRight) = foldMap f treeLeft <> f a <> foldMap f treeRight
  
instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node treeLeft a treeRight) =
    Node <$> (traverse f treeLeft) <*> (f a) <*> (traverse f treeRight)
