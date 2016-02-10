module Test where


data Foo a = Bar | Qux a deriving (Eq, Show)

instance Functor Foo where
  fmap _ Bar      = Bar
  fmap f (Qux a)  = Qux (f a)
