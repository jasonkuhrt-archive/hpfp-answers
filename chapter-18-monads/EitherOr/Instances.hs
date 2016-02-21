module Instances where



-- 1

data None a = None
  deriving (Eq, Show)

instance Functor None where
  fmap f None = None

instance Applicative None where
  pure _ = None
  (<*>) _ _ = None

instance Monad None where
  (>>=) _ _ = None



-- 3

data Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity z) = Identity (f z)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity z) =  Identity (f z)

instance Monad Identity where
  (>>=) (Identity z) f = f z



-- 4

data List a =
    List a (List a)
  | Empty
  deriving (Eq, Show)

instance Functor List where
  fmap _ Empty = Empty
  fmap f (List z zs) = List (f z) (fmap f zs)

instance Applicative List where
  pure z = List z Empty
  (<*>) Empty _ = Empty
  (<*>) _ Empty = Empty
  (<*>) (List f fs) (List z zs) = List (f z) ((<*>) fs zs)

instance Monoid (List a) where
  mempty = Empty
  mappend Empty zs = zs
  mappend (List z zs) xs  = List z (mappend zs xs)

instance Monad List where
  (>>=) Empty _       = Empty
  (>>=) (List z zs) f = mappend (f z) ((>>=) zs f)
