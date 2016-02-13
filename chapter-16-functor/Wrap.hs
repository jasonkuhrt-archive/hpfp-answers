module Wrap where



data Wrap f a =
  Wrap (f a)
  deriving (Eq, Show)

instance (Functor f) => Functor (Wrap f) where
  fmap f (Wrap a) = Wrap (fmap f a)
