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

main :: IO ()
main = do
  let trigger :: Identity (Int, Int, [Int])
      trigger = undefined
      constantTrigger :: Constant (Int, Int, [Int]) (Int, Int, [Int])
      constantTrigger = undefined
      optionalTrigger :: Optional (Int, Int, [Int])
      optionalTrigger = undefined
  quickBatch (traversable trigger)
  quickBatch (traversable constantTrigger)
  quickBatch (traversable optionalTrigger)
