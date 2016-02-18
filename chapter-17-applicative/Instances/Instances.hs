module Instances where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers


test :: IO ()
test = do
  quickBatch $ functor $ Identity ("","","")
  quickBatch $ applicative $ Identity ("","","")



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
