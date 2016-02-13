{-# LANGUAGE FlexibleInstances #-}

module Instances where

import Test.QuickCheck
import Test.QuickCheck.Function

type C = Char



-- Test Runner

main :: IO ()
main = do
  quickCheck (\x -> functorIdentity (x :: Quant C C))
  quickCheck (functorCompose :: Quant C C -> Fun C C -> Fun C C -> Bool)

  quickCheck (\x -> functorIdentity (x :: K C C))
  quickCheck (functorCompose :: K C C -> Fun C C -> Fun C C -> Bool)

  quickCheck (\x -> functorIdentity (x :: Flip Z C C))
  quickCheck (functorCompose :: Flip Z C C -> Fun C C -> Fun C C -> Bool)

  quickCheck (\x -> functorIdentity (x :: EvilGoateeConst C C))
  quickCheck (functorCompose :: EvilGoateeConst C C -> Fun C C -> Fun C C -> Bool)


-- Property Tests

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity a = id a == fmap id a

functorCompose :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
functorCompose a (Fun _ f) (Fun _ g) = fmap (g . f) a == (fmap g . fmap f) a



-- 1
data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance    = Finance
  fmap _ (Desk a)   = Desk a
  fmap f (Bloor a)  = Bloor (f a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Finance, Desk a, Bloor b]



-- 2
data K a b = K a
  deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a

instance (Arbitrary a) => Arbitrary (K a b) where
  arbitrary = do
    a <- arbitrary
    return (K a)



-- 3
-- Wow, this exercise is a bit a mindwarp. Wouldn't hurt to come back and
-- remember/think about this one once in a while until it feels
-- very comfortable.
newtype Flip f a b = Flip (f b a)
  deriving (Eq, Show)

newtype Z a b = Z a
  deriving (Eq, Show)

instance Functor (Flip Z a) where
  -- Because of Flip the following destructuring Z a is actually pointing to the `b` in type Z a b which is not part of the structure in the Functor definition
  fmap f (Flip (Z a)) = Flip (Z (f a))

instance (Arbitrary b) => Arbitrary (Flip Z a b) where
  arbitrary = do
    a <- arbitrary
    return (Flip (Z a))



-- 4
data EvilGoateeConst a b = GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst a) = GoatyConst (f a)

instance (Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = do
    a <- arbitrary
    return (GoatyConst a)



-- 5
-- TODO I don't know how to QuickCheck this
data LiftItOut f a = LiftItOut (f a)
  deriving (Eq, Show)

instance (Functor f) => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-- instance (Arbitrary f, Arbitrary a) => Arbitrary (LiftItOut f a) where
--   arbitrary = do
--     a <- arbitrary
--     b <- arbitrary
--     return (LiftItOut (a b))



-- 6
-- TODO I don't know how to QuickCheck this
data Parappa f g a = DaWrappa (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)



-- 7
-- TODO I don't know how to QuickCheck this
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
  deriving (Eq, Show)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)



-- 8
-- TODO I don't know how to QuickCheck this
data Notorious g o a t = Notorious (g o) (g a) (g t)
  deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)



-- 9
-- TODO I don't know how to QuickCheck this
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil          = Nil
  fmap f (Cons x xs)  = Cons (f x) (fmap f xs)



-- 10
-- TODO I don't know how to QuickCheck this
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat             = NoGoat
  fmap f (OneGoat a)        = OneGoat (f a)
  fmap f (MoreGoats a b c)  = MoreGoats (fmap f a) (fmap f b) (fmap f c)


-- 11
-- TODO I don't know how to QuickCheck this
data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt             = Halt
  fmap f (Print string a) = Print string (f a)
  fmap f (Read fn)        = Read (f . fn) -- Or (fmap f fn)
