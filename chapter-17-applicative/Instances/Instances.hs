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

  quickBatch $ monoid $ (undefined :: Two S (S,S,S))
  quickBatch $ functor $ (undefined :: Two S (S,S,S))
  quickBatch $ applicative $ (undefined :: Two S (S,S,S))

  quickBatch $ monoid $ (undefined :: Three S S (S,S,S))
  quickBatch $ functor $ (undefined :: Three S S (S,S,S))
  quickBatch $ applicative $ (undefined :: Three S S (S,S,S))



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




data Two a b = Two a b
  deriving (Eq, Show)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend (Two u s) (Two x z) = Two (mappend u x) (mappend s z)

instance Functor (Two a) where
  fmap f (Two x z) = Two x (f z)

instance (Monoid a) => Applicative (Two a) where
  pure x = Two mempty x
  (<*>) (Two u f) (Two x z) = Two (mappend u x) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    z <- arbitrary
    pure $ Two x z

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq



data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three u x z) = Three u x (f z)

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty
  mappend (Three v x z) (Three v' x' z') = Three (mappend v v') (mappend x x') (mappend z z')

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (<*>) (Three v x f) (Three v' x' z) = Three (mappend v v') (mappend x x') (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    v <- arbitrary
    x <- arbitrary
    z <- arbitrary
    pure $ Three v x z

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq
