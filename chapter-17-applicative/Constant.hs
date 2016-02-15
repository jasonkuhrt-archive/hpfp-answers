module Constant where



newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant b) = Constant b

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant _) <*> (Constant a) = Constant a
