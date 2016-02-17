module Validation where

import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes



data Validation err x =
    Failure err
  | Success x
  deriving (Eq, Show)

instance Functor (Validation a) where
  fmap _ (Failure x) = Failure x
  fmap f (Success x) = Success (f x)

instance (Monoid a) => Applicative (Validation a) where
  pure = Success
  (<*>) (Failure x) (Failure z) = Failure (mappend x z)
  (<*>) (Success f) (Success x) = Success (f x)
  (<*>) (Failure x) _ = Failure x
  (<*>) _ (Failure x) = Failure x




-- Test

type S = String

test :: IO ()
test = do
  quickBatch $ functor $ (undefined :: Validation S (S, S, S))
  quickBatch $ applicative $ (undefined :: Validation S (S, S, S))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    x <- arbitrary
    z <- arbitrary
    elements [Failure x, Success z]

instance (Eq a, Eq b) => EqProp (Validation a b) where
  (=-=) = eq
