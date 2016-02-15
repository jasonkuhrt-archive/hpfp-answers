module Identity where

import Control.Applicative



newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  (Identity f) <*> (Identity x) = Identity (f x)
  pure = Identity
