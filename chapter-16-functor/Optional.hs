module Optional where

import Test.QuickCheck
import Test.QuickCheck.Function

type FunCC = Fun Char Char



main :: IO ()
main = do
  quickCheck (\x -> functorIdentity (x :: Optional String))
  quickCheck (functorCompose :: Optional Char -> FunCC -> FunCC -> Bool)



-- Property Tests

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity a = id a == fmap id a

functorCompose :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
functorCompose a (Fun _ f) (Fun _ g) =
  (fmap g . fmap f) a == fmap (g . f) a



-- Custom Maybe Type

data Optional a =
    Nil
  | Thing a
  deriving (Eq, Show)

instance Functor Optional where
  fmap f (Thing a)  = Thing (f a)
  fmap _ Nil        = Nil

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    elements [Thing a, Nil]
