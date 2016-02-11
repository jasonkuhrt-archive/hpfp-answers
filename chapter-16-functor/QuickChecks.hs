module QuickChecks where

import Test.QuickCheck
import Test.QuickCheck.Function



type FunCC = Fun Char Char

(<|) :: (a -> b) -> a -> b
(<|) = ($)

(|>) :: a -> (a -> b) -> b
(|>) a f = f a



main :: IO ()
main = do
  quickCheck (\x -> functorIdentity (x :: Maybe String))
  -- quickCheck (\x -> functorCompose (++ "!") (++ "!") (x :: Maybe String))
  quickCheck (functorCompose' :: String -> FunCC -> FunCC -> Bool)

  quickCheck (\x -> functorIdentity (x :: Identity Char))
  quickCheck (functorCompose' :: Identity Char -> FunCC -> FunCC -> Bool)

  quickCheck (\x -> functorIdentity (x :: Pair Char))
  quickCheck (functorCompose' :: Pair Char -> FunCC -> FunCC -> Bool)

  quickCheck (\x -> functorIdentity (x :: Two Char Char))
  quickCheck (functorCompose' :: Two Char Char -> FunCC -> FunCC -> Bool)

  quickCheck (\x -> functorIdentity (x :: Three Char Char Char))
  quickCheck (functorCompose' :: Three Char Char Char -> FunCC -> FunCC -> Bool)

  quickCheck (\x -> functorIdentity (x :: Three' Char Char))
  quickCheck (functorCompose' :: Three' Char Char -> FunCC -> FunCC -> Bool)

  quickCheck (\x -> functorIdentity (x :: Four Char Char Char Char))
  quickCheck (functorCompose' :: Four Char Char Char Char -> FunCC -> FunCC -> Bool)

  quickCheck (\x -> functorIdentity (x :: Four' Char Char))
  quickCheck (functorCompose' :: Four' Char Char -> FunCC -> FunCC -> Bool)






-- Functor Property Tests

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity x = fmap id x == id x
--
-- functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
-- functorCompose f g x = (fmap g (fmap f x)) == fmap (g . f) x

functorCompose' :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap g . fmap f) x == fmap (g . f) x






-- 1

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = f a |> Identity

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary



-- 2

data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    Pair a b |> return



-- 3

data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = f b |> Two a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    Two a b |> return



-- 4

data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
   fmap f (Three a b c) = f c |> Three a b

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    Three a b c |> return



-- 5

data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    Three' a b c |> return



-- 6

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = f d |> Four a b c

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    Four a b c d |> return



-- 7

data Four' a b = Four' a b a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a (f b) c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    Four' a b c d |> return



-- 8 Is this implementable?

data Trivial = Trivial

-- No!
-- The kind of Trivial is * but Functor Typeclass expects a type
-- of kind * -> * . Trivial is pure structure, with no contents, and
-- therefore logically there is nothing to fmap.
