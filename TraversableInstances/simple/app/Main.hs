module Main where

import Lib

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return (Constant a)

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return Nada), (1, return (Yep a))]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    l <- arbitrary
    frequency [(1, return (Cons a l)), (1, return Nil)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return (Big a b1 b2)

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    b3 <- arbitrary
    return (Bigger a b1 b2 b3)

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    tl <- arbitrary
    tr <- arbitrary
    frequency [(1, return Empty), (1, return (Leaf a)), (1, return (Node tl a tr))]

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

main :: IO ()
main = do
  let trigger :: Identity (Int, Int, [Int])
      trigger = undefined
      constantTrigger :: Constant (Int, Int, [Int]) (Int, Int, [Int])
      constantTrigger = undefined
      optionalTrigger :: Optional (Int, Int, [Int])
      optionalTrigger = undefined
      listTrigger :: List (Int, Int, [Int])
      listTrigger = undefined
      threeTrigger :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Int, Int, [Int])
      threeTrigger = undefined
      pairTrigger :: Pair (Int, Int, [Int]) (Int, Int, [Int])
      pairTrigger = undefined
      bigTrigger :: Big (Int, Int, [Int]) (Int, Int, [Int])
      bigTrigger = undefined
      biggerTrigger :: Bigger (Int, Int, [Int]) (Int, Int, [Int])
      biggerTrigger = undefined
      treeTrigger :: Tree (Int, Int, [Int])
      treeTrigger = undefined
  quickBatch (traversable trigger)
  quickBatch (traversable constantTrigger)
  quickBatch (traversable optionalTrigger)
  quickBatch (traversable listTrigger)
  quickBatch (traversable threeTrigger)
  quickBatch (traversable pairTrigger)
  quickBatch (traversable bigTrigger)
  quickBatch (traversable biggerTrigger)
  quickBatch (traversable treeTrigger)
