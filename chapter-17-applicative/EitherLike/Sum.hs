module Sum where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes



-- Sum

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  (<*>) (First x) _ = First x
  (<*>) _ (First x) = First x
  (<*>) (Second f) (Second z) = Second (f z)



-- Test Runner

type S = String

test :: IO ()
test = do
  quickBatch $ functor $ (undefined :: Sum S (S, S, S))
  quickBatch $ applicative $ (undefined :: Sum S (S, S, S))



instance (Arbitrary b, Arbitrary a) => Arbitrary (Sum a b) where
  arbitrary = do
    x <- arbitrary
    z <- arbitrary
    elements [First x, Second z]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq
