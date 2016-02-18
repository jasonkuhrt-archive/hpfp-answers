module Instances where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers


type S = String
test :: IO ()
test = do
  quickBatch $ functor $ Identity ("","","")
  quickBatch $ applicative $ Identity ("","","")

  quickBatch $ functor $ (undefined :: Pair (S,S,S))
  quickBatch $ applicative $ (undefined :: Pair (S,S,S))



newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = pure Identity <*> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq



data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x z) = Pair (f x) (f z)

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f1 f2) (Pair x z) = Pair (f1 x) (f2 z)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    z <- arbitrary
    pure $ Pair x z

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq
