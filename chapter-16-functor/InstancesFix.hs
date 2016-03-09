module InstancesFix where



-- 1
data Sum a b =
    First b
  | Second a
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a)  = First (f a)
  fmap _ (Second b) = Second b



-- 2
data Company a b c =
    DeepBlue a b
  | Something c
  deriving (Eq, Show)

instance Functor (Company a b) where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c



-- 3
data More a b =
    L b a b
  | R a b a
  deriving (Eq, Show)

instance Functor (More a) where
  fmap f (L a b c) = L (f a) b (f c)
  fmap f (R a b c) = R a (f b) c
