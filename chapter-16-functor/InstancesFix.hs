module InstancesFix where



-- 1
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second (f b)



-- 2
data Company a b c =
    DeepBlue a c
  | Something b
  deriving (Eq, Show)

instance Functor (Company a b) where
  fmap f (DeepBlue a c) = DeepBlue a (f c)
  fmap _ (Something b) = Something b



-- 3
data More a b =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More a) where
  fmap f (L a b c) = L a (f b) c
  fmap f (R a b c) = R (f a) b (f c)
