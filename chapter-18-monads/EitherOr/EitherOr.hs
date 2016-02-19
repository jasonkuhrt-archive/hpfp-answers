module EitherOr where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type S = String



test :: IO ()
test = do
  quickBatch $ functor $ (undefined :: EitherOr S (S,S,S))
  quickBatch $ applicative $ (undefined :: EitherOr S (S,S,S))
  quickBatch $ monad $ (undefined :: EitherOr S (S,S,S))



data EitherOr a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (EitherOr a) where
  fmap _ (First z) = First z
  fmap f (Second z) = Second (f z)

instance Applicative (EitherOr a) where
  pure = Second
  (<*>) (First z) _ = First z
  (<*>) _ (First z) = First z
  (<*>) (Second f) (Second z) = Second (f z)

instance Monad (EitherOr a) where
  return = Second
  (>>=) (First z) _ = First z
  (>>=) (Second z) f = f z

instance (Arbitrary a, Arbitrary b) => Arbitrary (EitherOr a b) where
  arbitrary = do
    z <- arbitrary
    x <- arbitrary
    elements [First z, Second x]

instance (Eq a, Eq b) => EqProp (EitherOr a b) where
  (=-=) = eq
